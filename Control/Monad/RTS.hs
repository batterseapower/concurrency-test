{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, TypeFamilies, DoRec, DeriveDataTypeable, StandaloneDeriving, ExistentialQuantification #-}
module Control.Monad.RTS where

import Control.Applicative (Applicative(..))
import qualified Control.Exception as E
import Control.Monad
import qualified Control.Monad.Concurrent as MC
import Control.Monad.ST
import Control.Monad.ST.Class

import Data.Foldable (Foldable(foldMap))
import Data.List
import Data.Monoid (Monoid(..))
import Data.STRef
import qualified Data.STQueue as STQ
import Data.Traversable (Traversable(traverse))
import qualified Data.Traversable as Traversable
import Data.Typeable (Typeable(..), Typeable1(..), mkTyCon, mkTyConApp)

import Debug.Trace

import Test.LazySmallCheck hiding (Report(..))
import Test.QuickCheck hiding (Success, Result, (><))
import Test.QuickCheck.Gen

import System.Random
import System.IO.Unsafe

import Unsafe.Coerce (unsafeCoerce)

import Utilities
import Prelude hiding (catch)


instance Arbitrary StdGen where
    arbitrary = MkGen $ \gen _ -> gen
    shrink _ = []


-- I was initially inspired by Quviq/PULSE <http://www.protest-project.eu/upload/paper/icfp070-claessen.pdf>.
-- However the approach that I'm taking here is much more similar to that of the CHESS system
-- of Madanlal Musuvathi <http://research.microsoft.com/en-us/people/madanm/allpubs.aspx>


-- I used to use unsafeIsEvaluated to decide where to put in "...", but that pruned too heavily because
-- I couldn't show the schedule before it was actually poked on and those thunks turned into real values.
{-# NOINLINE showsExplored #-}
showsExplored :: (a -> ShowS) -> a -> ShowS
showsExplored shows x = unsafePerformIO $ fmap (maybe (showString "...") shows) $ tryIf isLSCError (E.evaluate x)
  where
    -- Looked at the LSC code to see what sort of errors it was generating...
    isLSCError (E.ErrorCall ('\0':_)) = True
    isLSCError _                      = False
    
    tryIf :: E.Exception e => (e -> Bool) -> IO a -> IO (Maybe a)
    tryIf p act = fmap (either (\() -> Nothing) Just) $ E.tryJust (\e -> guard (p e) >> return ()) act


instance Serial Nat where
    series d = drawnFrom $ map Nat [0..d]


data Stream a = a :< Stream a

instance Show a => Show (Stream a) where
    showsPrec d = showsExplored (\(x :< xs) -> showParen (d > 0) $ showsPrec 1 x . showString " :< " . showsPrec 1 xs)

instance Serial a => Serial (Stream a) where
    series = cons2 (:<)

genericIndexStream :: Num i => Stream a -> i -> a
genericIndexStream (x :< xs) n = if n == 0 then x else genericIndexStream xs (n - 1)


-- | A stream suitable for use for guiding the scheduler. The natural number stored in the nth element
-- of one of the Stream (Stream Nat) we contain is drawn uniformly from the range [0,n].
--
-- In one use of the scheduler, all but one element of each Stream will be discarded, since they correspond
-- to schedulings for executions with more or less pending processes than we actually saw
newtype SchedulerStream = SS { unSS :: Stream (Nat, SchedulerStream) }
                        deriving (Show)

instance Serial SchedulerStream where
    series = streamSeries0
      where
        streamSeries' :: Nat -> Series (Nat, SchedulerStream)
        streamSeries' n = (cons (,) >< (\_ -> drawnFrom [0..n]) >< streamSeries0) . (+1)
    
        streamSeries0 :: Series SchedulerStream
        streamSeries0 = streamSeriesN 0
    
        streamSeriesN :: Nat -> Series SchedulerStream
        streamSeriesN n = cons (\n ss -> SS ((:<) n (unSS ss))) >< streamSeries' n >< streamSeriesN (n + 1)


newtype Scheduler = Scheduler { schedule :: Nat              -- ^ One less than the number of pending processes (n)
                                         -> (Scheduler, Nat) -- ^ Pending process to run, and the next scheduler (0-based index, so valid values are from 0 to n inclusive)
                              }

unfair :: Scheduler
unfair = Scheduler schedule
  where schedule _ = (unfair, 0)

roundRobin :: Scheduler
roundRobin = Scheduler schedule
  where schedule n = (roundRobin, n)

streamed :: Stream Nat -> Scheduler
streamed (i :< is) = Scheduler schedule
  where schedule n = (streamed is, i `mod` n) -- A bit unsatisfactory because I really want a uniform chance of scheduling the available threads

schedulerStreamed :: SchedulerStream -> Scheduler
schedulerStreamed (SS sss) = Scheduler schedule
  where schedule n = (schedulerStreamed sss', i)
          where (i, sss') = genericIndexStream sss n

randomised :: StdGen -> Scheduler
randomised gen = Scheduler schedule
  where schedule n = (randomised gen', i)
          where (i, gen') = randomR (0, n) gen

instance Show Scheduler where
    show _ = "Scheduler" -- TODO: have to be able to show failing schedulings in a nice way

instance Arbitrary Scheduler where
    arbitrary = fmap randomised arbitrary
    shrink _ = []

instance Serial Scheduler where
    series = cons schedulerStreamed >< series

-- TODO: think about what happens if we get something other than Success on a non-main thread.
-- I'm not even sure what the current behaviour is, but I think it stops us immediately.
data Result a = Success a
              | BlockedIndefinitely
              | UnhandledException E.SomeException
              deriving (Show)

instance Eq a => Eq (Result a) where
    Success x1            == Success x2            = x1 == x2
    BlockedIndefinitely   == BlockedIndefinitely   = True
    UnhandledException e1 == UnhandledException e2 = show e1 == show e2
    _                     == _                     = False

instance Functor Result where
    fmap = liftM

instance Applicative Result where
    pure = return
    (<*>) = ap

instance Monad Result where
    return = Success
    Success x              >>= f = f x
    BlockedIndefinitely    >>= _ = BlockedIndefinitely
    (UnhandledException e) >>= _ = UnhandledException e

instance Foldable Result where
    foldMap f (Success x)            = f x
    foldMap _ BlockedIndefinitely    = mempty
    foldMap _ (UnhandledException _) = mempty

instance Traversable Result where
    traverse f (Success x)            = pure Success <*> f x
    traverse _ BlockedIndefinitely    = pure BlockedIndefinitely
    traverse _ (UnhandledException e) = pure (UnhandledException e)


data SyncObject s r = forall a. SyncMVar     (MVar s r a)
                    |           SyncThreadId (ThreadId s r)

instance Eq (SyncObject s r) where
    SyncMVar mvar1    == SyncMVar mvar2    = mvar1 == unsafeCoerce mvar2 -- Grr... nonetheless safe since (==) cannot look at the element (free theorem)
    SyncThreadId tid1 == SyncThreadId tid2 = tid1 == tid2
    _                 == _                 = False


newtype SetEq a = SetEq { unSetEq :: [a] }

instance Eq a => Eq (SetEq a) where
    se1 == se2 = all (`elem` unSetEq se2) (unSetEq se1) -- This is correct because elements in both sets must be unique

emptySetEq :: SetEq a
emptySetEq = SetEq []

unionSetEq :: Eq a => SetEq a -> SetEq a -> SetEq a
unionSetEq se1 se2 = SetEq $ nub $ unSetEq se1 ++ unSetEq se2

instance Eq a => Monoid (SetEq a) where
    mempty = emptySetEq
    mappend = unionSetEq


-- | A pending coroutine
--
-- You almost always want to use (scheduleM ((tid, throw, k) : resumables)) rather than (unPending k resumables) because
-- scheduleM gives QuickCheck a chance to try other schedulings, whereas using unPending forces control
-- flow to continue in the current thread.
type Resumable s r = (ThreadId s r, Unwinder s r, Pending s r)
newtype Pending s r = Pending { unPending :: [Resumable s r]        -- ^ Runnable threads that are eligible for execution, and their multi-step unwinders. Everything in this list is Uninterruptible.
                                          -> Nat                    -- ^ Next ThreadId to allocate
                                          -> Scheduler              -- ^ Current scheduler, used for at the next rescheduling point
                                          -> ST s (Result r) } -- TODO: Pending is itself almost a monad
newtype RTS s r a = RTS { unRTS :: (a -> Pending s r)            -- ^ Continuation: how we should continue after we have our result
                                -> ThreadId s r                  -- ^ ThreadId of this thread of execution
                                -> STQ.STQueue s (Suspended s r) -- ^ Suspended threads that may or may not have been resumed yet: this is necessary because we may want to deliver asyncronous exceptions to them. As such, everything in this list is Interruptible.
                                -- -> SetEq (SyncObject s r)        -- ^ Overapproximation of the synchronisation objects this thread closes over
                                -> Unwinder s r                  -- ^ Exception handling continuation
                                -> Pending s r }
-- | We have to be able to throw several exceptions in succession because we can have more than one pending asynchronous exceptions.
data Unwinder s r = Unwinder {
    masking         :: E.MaskingState, -- ^ Where we stand wrt. asynchronous exceptions
    uncheckedUnwind :: E.SomeException
                    -> ST s (Interruptibility,
                             Unwinder s r,
                             Pending s r)
  }

maskUnwinder :: Unwinder s r -> Interruptibility -> Unwinder s r
maskUnwinder throw Interruptible   = throw { masking = case masking throw of E.MaskedUninterruptible -> E.MaskedUninterruptible; _ -> E.MaskedInterruptible }
maskUnwinder throw Uninterruptible = throw { masking = E.MaskedUninterruptible }


unwindAsync :: Unwinder s r -> Interruptibility -> Maybe (E.SomeException -> ST s (Interruptibility, Unwinder s r, Pending s r))
unwindAsync throw interruptible = guard (canThrow (masking throw) interruptible) >> return (uncheckedUnwind throw)

unwindSync :: Unwinder s r -> E.SomeException -> Pending s r
unwindSync throw e = Pending $ \resumables next_tid scheduler -> do
    pending <- fmap thd3 (uncheckedUnwind throw e)
    unPending pending resumables next_tid scheduler

runRTS :: Scheduler -> (forall s. RTS s r r) -> Result r
runRTS scheduler mx = runST $ do
    tid <- newThreadId 0
    suspendeds <- STQ.new
    unPending (unRTS mx (\x -> Pending $ \_resumables _next_tid _scheduler -> return (Success x)) tid suspendeds (unhandledException E.Unmasked)) [] 1 scheduler

unhandledException :: E.MaskingState -> Unwinder s r
unhandledException masking = Unwinder {
    masking = masking,
    uncheckedUnwind = \e -> return (Uninterruptible {- Don't think it matters either way -}, unhandledException (E.Unmasked), Pending $ \_resumables _next_tid _scheduler -> return (UnhandledException e)) -- TODO: we only report the last unhandled exception. Could do better?
  }


instance Functor (RTS s r) where
    fmap = liftM

instance Applicative (RTS s r) where
   pure = return
   (<*>) = ap

instance Monad (RTS s r) where
    return x = RTS $ \k _tid _suspendeds _throw -> k x
    mx >>= fxmy = RTS $ \k_y tid suspendeds throw -> unRTS mx (\x -> unRTS (fxmy x) k_y tid suspendeds throw) tid suspendeds throw

instance MC.MonadException (RTS s r) where
    mask = mask
    uninterruptibleMask = uninterruptibleMask
    getMaskingState = getMaskingState
    
    throwIO = throwIO
    throwTo = throwTo
    catch = catch

instance MC.MonadConcurrent (RTS s r) where
    type MC.ThreadId (RTS s r) = ThreadId s r

    forkIO = forkIO
    myThreadId = myThreadId

    yield = yield

data Interruptibility = Interruptible | Uninterruptible

mask :: ((forall a. RTS s r a -> RTS s r a) -> RTS s r b) -> RTS s r b
mask = maskWith Interruptible

uninterruptibleMask :: ((forall a. RTS s r a -> RTS s r a) -> RTS s r b) -> RTS s r b
uninterruptibleMask = maskWith Uninterruptible

getMaskingState :: RTS s r E.MaskingState
getMaskingState = RTS $ \k _tid _suspendeds throw -> k (masking throw)

maskWith :: Interruptibility -> ((forall a. RTS s r a -> RTS s r a) -> RTS s r b) -> RTS s r b
maskWith interruptible while = RTS $ \k tid suspendeds throw -> unRTS (while (\unmask -> RTS $ \k' tid' suspendeds' throw' -> unRTS unmask k' tid' suspendeds' throw' { masking = masking throw })) (\b -> Pending $ \resumables next_tid scheduler -> scheduleM suspendeds ((tid, throw, k b) : resumables) next_tid scheduler) tid suspendeds (throw `maskUnwinder` interruptible)
  where
    -- NB: must call scheduleM after exiting the masked region so we can pump asynchronous exceptions that may have accrued while masked
    -- TODO: I think it would be safe to do scheduleM iff there were actually some exceptions on this thread to pump which are *newly enabled*
    -- by the transition in masking states: this would help reduce the scheduler search space

throwIO :: E.Exception e => e -> RTS s r a
throwIO e = RTS $ \_k _tid _suspendeds throw -> unwindSync throw (E.SomeException e)

throwTo :: E.Exception e => ThreadId s r -> e -> RTS s r ()
throwTo target_tid e = RTS $ \k tid suspendeds throw -> if target_tid == tid
                                                         then unwindSync throw (E.SomeException e) -- See GHC #4888: we always throw an exception regardless of the mask mode
                                                         else Pending $ \resumables next_tid scheduler -> do
                                                           -- If we ourselves get interrupted by an asynchronous exception before the one we sent was delivered,
                                                           -- recover by still delivering the exception but ensure that doing so does not cause the pending list to change
                                                           rec { let kill_suspended = STQ.delete interrupt_loc >>= \(Just _) -> return ()
                                                               ; kill_blocked <- enqueueAsyncException target_tid (E.SomeException e) (tid, throw, k ()) kill_suspended
                                                               ; interrupt_loc <- flip STQ.enqueue suspendeds (tid, throw { uncheckedUnwind = \e -> kill_blocked >> uncheckedUnwind throw e })
                                                               ; return () }
                                                           scheduleM suspendeds resumables next_tid scheduler

catch :: E.Exception e => RTS s r a -> (e -> RTS s r a) -> RTS s r a
catch mx handle = RTS $ \k tid suspendeds throw -> unRTS mx k tid suspendeds $ throw { uncheckedUnwind = \e -> maybe (uncheckedUnwind throw e) (\e -> return (Uninterruptible, throw, unRTS (handle e) k tid suspendeds throw)) (E.fromException e) }

-- | Give up control to the scheduler. Control is automatically given up to the scheduler after calling every RTS primitive
-- which might have effects observable outside the current thread. This is enough to almost guarantee that there exists some
-- scheduler for which the RTS monad will give the same results as the IO monad.
--
-- The exception to this guarantee is if you write a non-terminating computation on a thread (other than the initial thread) which
-- does not call any RTS primitive that gives up control to the scheduler. For such computations, you need to manually add a
-- call to 'yield' to allow the scheduler to interrupt the loop.
yield :: RTS s r ()
yield = RTS $ \k tid suspendeds throw -> Pending $ \resumables -> scheduleM suspendeds ((tid, throw, k ()) : resumables)
  -- It is certainly enough to yield on every bind operation. But this is too much (and it breaks the monad laws).
  -- Quviq/PULSE yields just before every side-effecting operation. I think we can just yield after every side-effecting
  -- operation and get the same results.


-- TODO: rethink treatment of asynchronous exceptions.. for one thing we are not generating enough schedulings
-- TODO: it might be cool to have a mode that generates random asynchronous exceptions to try to crash other threads
scheduleM :: STQ.STQueue s (Suspended s r) -> [Resumable s r] -> Nat -> Scheduler -> ST s (Result r)
scheduleM suspendeds resumables next_tid scheduler = do
    -- Deliver asynchronous exceptions to suspended threads (if they have any such exceptions pending).
    -- This is the only mechanism that lets such threads wake up, bar the blocking call resuming normally.
    ((), resumables) <- runWriterST $ (>>) (tell resumables) $ flip STQ.mapMaybeM suspendeds $ \suspended -> do
        mb_resumables <- liftST $ dequeueAsyncExceptionsOnBlocked suspended
        case mb_resumables of
          Nothing         -> return (Just suspended)
          Just resumables -> tell resumables >> return Nothing
    case resumables of
      [] -> return BlockedIndefinitely
      _  -> do
          -- Delivers asynchoronous exceptions to the chosen resumable ONLY
          (pending, resumables'') <- dequeueAsyncExceptions tid (Uninterruptible, throw, pending)
          unPending pending (resumables'' ++ resumables') next_tid scheduler'
        where (scheduler', i) = schedule scheduler (genericLength resumables - 1)
              ((tid, throw, pending), resumables') = genericDeleteAt resumables i


newtype WriterST s m a = WriterST { runWriterST :: ST s (a, m) }

instance Monoid m => Functor (WriterST s m) where
    fmap = liftM

instance Monoid m => Applicative (WriterST s m) where
    pure = return
    (<*>) = ap

instance Monoid m => Monad (WriterST s m) where
    return x = WriterST $ return (x, mempty)
    mx >>= fxmy = WriterST $ do
      (x, m1) <- runWriterST mx
      (y, m2) <- runWriterST (fxmy x)
      return (y, m1 `mappend` m2)

instance Monoid m => MonadST (WriterST s m) where
    type StateThread (WriterST s m) = s
    liftST st = WriterST $ fmap (flip (,) mempty) st

tell :: Monoid m => m -> WriterST s m ()
tell m = WriterST $ return ((), m)


instance MonadST (RTS s r) where
    type StateThread (RTS s r) = s
    liftST st = RTS $ \k _tid _suspendeds _throw -> Pending $ \resumables next_tid scheduler -> st >>= \x -> unPending (k x) resumables next_tid scheduler
        
canThrow :: E.MaskingState -> Interruptibility -> Bool
canThrow E.Unmasked            _             = True
canThrow E.MaskedInterruptible Interruptible = True
canThrow _                     _             = False

dequeueAsyncExceptions :: ThreadId s r -> (Interruptibility, Unwinder s r, Pending s r) -> ST s (Pending s r, [Resumable s r])
dequeueAsyncExceptions tid = go []
  where
    -- TODO: currently I always unwind absolutely every available exception.
    -- This might mask some bugs, so we might want to just unwind a (possibly empty) prefix.
    go resumables (interruptibility, throw, pending) = do
      case unwindAsync throw interruptibility of
         -- Cannot unwind this thread right now due to masking
        Nothing -> return (pending, resumables)
        Just unchecked_unwind -> do
          mb_e <- dequeueAsyncException tid
          case mb_e of
             -- No exception available to actually unwind with
            Nothing                -> return (pending, resumables)
            Just (e, mb_resumable) -> unchecked_unwind e >>= go (maybe id (:) mb_resumable resumables)

-- TODO: currently I always unwind a pending exception. Similarly to above, I could choose not to unwind for a little bit
dequeueAsyncExceptionsOnBlocked :: Suspended s r -> ST s (Maybe [Resumable s r])
dequeueAsyncExceptionsOnBlocked (tid, throw) = do
    case unwindAsync throw Interruptible of
       -- Cannot unwind this (blocked) thread right now due to masking
      Nothing -> return Nothing
      Just unchecked_unwind -> do
        mb_e <- dequeueAsyncException tid
        case mb_e of
           -- No exception available to actually unwind with
          Nothing                -> return Nothing
          Just (e, mb_resumable) -> do
             (pending, resumables) <- unchecked_unwind e >>= dequeueAsyncExceptions tid
             return $ Just $ (tid, throw, pending) : maybe id (:) mb_resumable resumables

-- FIXME (comment) newtype Interrupter s r = Interrupter { interrupt :: ST s () } -- NB: must be used as a one-shot thing (once you grab the unwinder, the thread will be dumped from the suspended position and enqueued as pending by the user of Suspended)
type Suspended s r = (ThreadId s r, Unwinder s r)

data ThreadId s r = ThreadId Nat (STRef s (Queue (E.SomeException, STRef s (Maybe (ST s (Resumable s r))))))

instance Eq (ThreadId s r) where
    ThreadId n1 _ == ThreadId n2 _ = n1 == n2

instance Ord (ThreadId s r) where
    ThreadId n1 _ `compare` ThreadId n2 _ = n1 `compare` n2

instance Show (ThreadId s r) where
    show (ThreadId n _) = show n

instance Typeable (ThreadId s r) where
    typeOf _ = mkTyConApp (mkTyCon "Control.Monad.RTS.ThreadId") []

newThreadId :: Nat -> ST s (ThreadId s r)
newThreadId tid = fmap (ThreadId tid) $ newSTRef emptyQueue

enqueueAsyncException :: ThreadId s r -> E.SomeException -> Resumable s r -> ST s () -> ST s (ST s ())
enqueueAsyncException (ThreadId _ ref) e resumable notify_block_complete = do
    asyncs <- readSTRef ref
    resumable_ref <- newSTRef $ Just $ notify_block_complete >> return resumable
    writeSTRef ref (queue (e, resumable_ref) asyncs)
    return $ readSTRef resumable_ref >>= \(Just _) -> writeSTRef resumable_ref Nothing

dequeueAsyncException :: ThreadId s r -> ST s (Maybe (E.SomeException, Maybe (Resumable s r)))
dequeueAsyncException (ThreadId _ ref) = do
    asyncs <- readSTRef ref
    case dequeue asyncs of
      Nothing                            -> return Nothing
      Just ((e, resumable_ref), asyncs') -> writeSTRef ref asyncs' >> readSTRef resumable_ref >>= \mb_get_resumable -> Traversable.sequence mb_get_resumable >>= \mb_resumable -> return (Just (e, mb_resumable))


forkIO :: RTS s r () -> RTS s r (MC.ThreadId (RTS s r))
forkIO forkable = RTS $ \k tid suspendeds throw -> Pending $ \resumables next_tid scheduler -> newThreadId next_tid >>= \tid' -> scheduleM suspendeds ((tid, throw, k tid') : (tid', unhandledException (masking throw), unRTS forkable (\() -> Pending (scheduleM suspendeds)) tid' suspendeds (unhandledException (masking throw))) : resumables) (next_tid + 1) scheduler

myThreadId :: RTS s r (MC.ThreadId (RTS s r))
myThreadId = RTS $ \k tid _suspendeds _throw -> k tid


-- TODO: I could detect more unreachable states if I find that a MVar currently blocking a Pending gets GCed
data MVar s r a = MVar {
    mvar_data :: STRef s (Maybe a),
    -- MVars have guaranteed FIFO semantics, hence the queues
    mvar_putters :: STQ.STQueue s (STQ.Location s (Suspended s r), a, Resumable s r),
    mvar_takers  :: STQ.STQueue s (STQ.Location s (Suspended s r), a -> Resumable s r)
  }

deriving instance Eq (MVar s r a)

instance Typeable1 (MVar s r) where
    typeOf1 _ = mkTyConApp (mkTyCon "Control.Monad.RTS.MVar") []

newEmptyMVar :: RTS s r (MVar s r a)
newEmptyMVar = newMVarInternal Nothing

newMVar :: a -> RTS s r (MVar s r a)
newMVar = newMVarInternal . Just

newMVarInternal :: Maybe a -> RTS s r (MVar s r a)
newMVarInternal mb_x = RTS $ \k _tid _suspendeds _throw -> Pending $ \resumables next_tid scheduler -> do
    data_ref <- newSTRef mb_x
    putter_queue <- STQ.new
    taker_queue <- STQ.new
    -- NB: unPending legitimate here because newMVarInternal cannot have externally visible side effects
    unPending (k (MVar data_ref putter_queue taker_queue)) resumables next_tid scheduler

takeMVar :: MVar s r a -> RTS s r a
takeMVar mvar = RTS $ \k tid suspendeds throw -> Pending $ \resumables next_tid scheduler -> do
    dat <- readSTRef (mvar_data mvar)
    case dat of
       -- NB: we must guarantee that the woken thread doing a putMVar (if any) completes its operation since takeMVar has this guarantee
      Just x  -> do
          (dat', resumables') <- STQ.dequeue (mvar_putters mvar) >>= \it -> case it of
              Nothing                         -> return (Nothing, resumables)
              Just (interrupt_loc, x, putter) -> STQ.delete interrupt_loc >>= \(Just _) -> return (Just x, putter : resumables)
          writeSTRef (mvar_data mvar) dat'
          scheduleM suspendeds ((tid, throw, k x) : resumables') next_tid scheduler
      Nothing -> do
          rec { success_loc <- STQ.enqueue (interrupt_loc, \x -> (tid, throw, k x)) (mvar_takers mvar)
                -- If we are interrupted, an asynchronous exception won the race: make sure that the standard wakeup loses
              ; interrupt_loc <- flip STQ.enqueue suspendeds (tid, throw { uncheckedUnwind = \e -> STQ.delete success_loc >>= \(Just _) -> uncheckedUnwind throw e })
              ; return () }
          scheduleM suspendeds resumables next_tid scheduler

putMVar :: MVar s r a -> a -> RTS s r ()
putMVar mvar x = RTS $ \k tid suspendeds throw -> Pending $ \resumables next_tid scheduler -> do
    dat <- readSTRef (mvar_data mvar)
    case dat of
       -- NB: we must guarantee that the woken thread doing a takeMVar (if any) completes its operation since putMVar has this guarantee
      Nothing -> do
          (dat', resumables') <- STQ.dequeue (mvar_takers mvar) >>= \it -> case it of
              Nothing                            -> return (Just x, resumables)
              Just (interrupt_loc, mk_resumable) -> STQ.delete interrupt_loc >>= \(Just _) -> return (Nothing, mk_resumable x : resumables)
          writeSTRef (mvar_data mvar) dat'
          scheduleM suspendeds ((tid, throw, k ()) : resumables') next_tid scheduler
      Just x  -> do
          rec { success_loc <- STQ.enqueue (interrupt_loc, x, (tid, throw, k ())) (mvar_putters mvar)
                -- If we are interrupted, an asynchronous exception won the race: make sure that the standard wakeup loses
              ; interrupt_loc <- flip STQ.enqueue suspendeds (tid, throw { uncheckedUnwind = \e -> STQ.delete success_loc >>= \(Just _) -> uncheckedUnwind throw e })
              ; return () }
          scheduleM suspendeds resumables next_tid scheduler


example1 :: RTS s r Integer
example1 = do
    yield
    v <- newEmptyMVar
    --putMVar v 3
    putMVar v 3
    yield
    --takeMVar v
    takeMVar v

example2 :: RTS s r Integer
example2 = do
    v_in <- newMVar 1336
    v_out <- newEmptyMVar
    _ <- forkIO $ do
        x <- takeMVar v_in
        yield
        putMVar v_out (x + 1)
    takeMVar v_out

-- An example with a race: depending on scheduling, this either returns "Hello" or "World"
example3 :: RTS s r String
example3 = do
    v <- newEmptyMVar
    _ <- forkIO $ putMVar v "Hello"
    _ <- forkIO $ putMVar v "World"
    takeMVar v


testScheduleSafe :: Eq r => (forall s. RTS s r r) -> IO ()
-- Cuter:
--testScheduleSafe act = test $ \sched -> expected == runRTS sched act
-- More flexible:
testScheduleSafe act = test $ \ss -> trace (show ss) $ expected == runRTS (schedulerStreamed ss) act
-- Working:
--testScheduleSafe act = quickCheck $ \gen -> expected == runRTS (randomised gen) act
  where expected = runRTS roundRobin act


main :: IO ()
main = do
    -- Demonstrates the presence of a data race - these two invocations return different results
    print $ runRTS unfair example3
    print $ runRTS roundRobin example3

    -- Let's see if we can find the race automatically!
    testScheduleSafe example3

