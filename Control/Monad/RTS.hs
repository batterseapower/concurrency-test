{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, TypeFamilies, DoRec #-}
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
import Data.Typeable (Typeable(..), mkTyCon, mkTyConApp)

import Debug.Trace

import Test.LazySmallCheck hiding (Report(..))
import Test.QuickCheck hiding (Success, Result, (><))
import Test.QuickCheck.Gen

import System.Random
import System.IO.Unsafe

import Utilities
import Prelude hiding (catch)


instance Arbitrary StdGen where
    arbitrary = MkGen $ \gen _ -> gen
    shrink _ = []


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


-- | A pending coroutine
--
-- You almost always want to use (scheduleM ((tid, throw, k) : resumables)) rather than (unPending k resumables) because
-- scheduleM gives QuickCheck a chance to try other schedulings, whereas using unPending forces control
-- flow to continue in the current thread.
type Resumable s r = (ThreadId s r, E.MaskingState, Unwinder s r, Pending s r)
newtype Pending s r = Pending { unPending :: [Resumable s r]   -- ^ Runnable threads that are eligible for execution, and their multi-step unwinders. Everything in this list is Uninterruptible.
                                          -> Nat               -- ^ Next ThreadId to allocate
                                          -> Scheduler         -- ^ Current scheduler, used for at the next rescheduling point
                                          -> ST s (Result r) } -- TODO: Pending is itself almost a monad
newtype RTSM s r a = RTSM { unRTSM :: (a -> Pending s r)            -- ^ Continuation: how we should continue after we have our result
                                   -> ThreadId s r                  -- ^ ThreadId of this thread of execution
                                   -> E.MaskingState                -- ^ Where we stand wrt. asynchronous exceptions
                                   -> STQ.STQueue s (Suspended s r) -- ^ Suspended threads that may or may not have been resumed yet: this is necessary because we may want to deliver asyncronous exceptions to them. As such, everything in this list is Interruptible.
                                   -> Unwinder s r                  -- ^ Exception handling continuation
                                   -> Pending s r }
-- | We have to be able to throw several exceptions in succession because we can have more than one pending asynchronous exceptions.
newtype Unwinder s r = Unwinder { unwind :: E.SomeException -> (E.MaskingState, Interruptibility, Unwinder s r, Pending s r) }

runRTSM :: Scheduler -> (forall s. RTSM s r r) -> Result r
runRTSM scheduler mx = runST $ do
    tid <- newThreadId 0
    suspendeds <- STQ.new
    unPending (unRTSM mx (\x -> Pending $ \_resumables _next_tid _scheduler -> return (Success x)) tid E.Unmasked suspendeds unhandledException) [] 1 scheduler

unhandledException :: Unwinder s r
unhandledException = Unwinder $ \e -> (E.Unmasked, Uninterruptible {- Don't think it matters either way -}, unhandledException, Pending $ \_resumables _next_tid _scheduler -> return (UnhandledException e)) -- TODO: we only report the last unhandled exception. Could do better?


instance Functor (RTSM s r) where
    fmap = liftM

instance Applicative (RTSM s r) where
   pure = return
   (<*>) = ap

instance Monad (RTSM s r) where
    return x = RTSM $ \k _tid _masking _suspendeds _throw -> k x
    mx >>= fxmy = RTSM $ \k_y tid masking suspendeds throw -> unRTSM mx (\x -> unRTSM (fxmy x) k_y tid masking suspendeds throw) tid masking suspendeds throw

instance MC.MonadException (RTSM s r) where
    mask = mask
    uninterruptibleMask = uninterruptibleMask
    getMaskingState = getMaskingState
    
    throwIO = throwIO
    throwTo = throwTo
    catch = catch

instance MC.MonadConcurrent (RTSM s r) where
    type MC.ThreadId (RTSM s r) = ThreadId s r

    forkIO = forkIO
    myThreadId = myThreadId

    yield = yield

data Interruptibility = Interruptible | Uninterruptible

mask :: ((forall a. RTSM s r a -> RTSM s r a) -> RTSM s r b) -> RTSM s r b
mask = maskWith Interruptible

uninterruptibleMask :: ((forall a. RTSM s r a -> RTSM s r a) -> RTSM s r b) -> RTSM s r b
uninterruptibleMask = maskWith Uninterruptible

getMaskingState :: RTSM s r E.MaskingState
getMaskingState = RTSM $ \k _tid masking _suspendeds _throw -> k masking

maskWith :: Interruptibility -> ((forall a. RTSM s r a -> RTSM s r a) -> RTSM s r b) -> RTSM s r b
maskWith interruptible while = RTSM $ \k tid masking suspendeds throw -> unRTSM (while (\unmask -> RTSM $ \k tid _masking -> unRTSM unmask k tid masking)) (\b -> Pending $ \resumables next_tid scheduler -> scheduleM suspendeds ((tid, masking, throw, k b) : resumables) next_tid scheduler) tid masking' suspendeds throw
  where
    -- NB: must call scheduleM after exiting the masked region so we can pump asynchronous exceptions that may have accrued while masked
    -- TODO: I think it would be safe to do scheduleM iff there were actually some exceptions on this thread to pump which are *newly enabled*
    -- by the transition in masking states: this would help reduce the scheduler search space
    masking' = case interruptible of Uninterruptible -> E.MaskedUninterruptible
                                     Interruptible   -> E.MaskedInterruptible

throwIO :: E.Exception e => e -> RTSM s r a
throwIO e = RTSM $ \_k _tid _masking _suspendeds throw -> fth4 (unwind throw (E.SomeException e))

throwTo :: E.Exception e => ThreadId s r -> e -> RTSM s r ()
throwTo target_tid e = RTSM $ \k tid masking suspendeds throw -> if target_tid == tid
                                                                  then fth4 (unwind throw (E.SomeException e)) -- See GHC #4888: we always throw an exception regardless of the mask mode
                                                                  else Pending $ \resumables next_tid scheduler -> do
                                                                    -- If we ourselves get interrupted by an asynchronous exception before the one we sent was delivered,
                                                                    -- recover by still delivering the exception but ensure that doing so does not cause the pending list to change
                                                                    rec { let kill_suspended = STQ.delete interrupt_loc >>= \(Just _) -> return ()
                                                                        ; kill_blocked <- enqueueAsyncException target_tid (E.SomeException e) (tid, masking, throw, k ()) kill_suspended
                                                                        ; interrupt_loc <- flip STQ.enqueue suspendeds (tid, masking, Interrupter $ kill_blocked >> return throw)
                                                                        ; return () }
                                                                    scheduleM suspendeds resumables next_tid scheduler

catch :: E.Exception e => RTSM s r a -> (e -> RTSM s r a) -> RTSM s r a
catch mx handle = RTSM $ \k tid masking suspendeds throw -> unRTSM mx k tid masking suspendeds $ Unwinder $ \e -> maybe (unwind throw e) (\e -> (masking, Uninterruptible, throw, unRTSM (handle e) k tid masking suspendeds throw)) (E.fromException e)

-- | Give up control to the scheduler: yields should be used at every point where it is useful to allow QuickCheck
-- to try several different scheduling options.
--
-- It is certainly enough to yield on every bind operation. But this is too much (and it breaks the monad laws).
-- Quviq/PULSE yields just before every side-effecting operation. I think we can just yield after every side-effecting
-- operation and get the same results.
yield :: RTSM s r ()
yield = RTSM $ \k tid masking suspendeds throw -> Pending $ \resumables -> scheduleM suspendeds ((tid, masking, throw, k ()) : resumables)

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
          (pending, resumables'') <- dequeueAsyncExceptions tid (masking, Uninterruptible, throw, pending)
          unPending pending (resumables'' ++ resumables') next_tid scheduler'
        where (scheduler', i) = schedule scheduler (genericLength resumables - 1)
              ((tid, masking, throw, pending), resumables') = genericDeleteAt resumables i


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


instance MonadST (RTSM s r) where
    type StateThread (RTSM s r) = s
    liftST st = RTSM $ \k _tid _masking _suspendeds _throw -> Pending $ \resumables next_tid scheduler -> st >>= \x -> unPending (k x) resumables next_tid scheduler
        
canThrow :: E.MaskingState -> Interruptibility -> Bool
canThrow E.Unmasked            _             = True
canThrow E.MaskedInterruptible Interruptible = True
canThrow _                     _             = False

dequeueAsyncExceptions :: ThreadId s r -> (E.MaskingState, Interruptibility, Unwinder s r, Pending s r) -> ST s (Pending s r, [Resumable s r])
dequeueAsyncExceptions tid = go []
  where
    -- TODO: currently I always unwind absolutely every available exception.
    -- This might mask some bugs, so we might want to just unwind a (possibly empty) prefix.
    go resumables (masking, interruptibility, throw, pending)
      | canThrow masking interruptibility = do
        mb_e <- dequeueAsyncException tid
        case mb_e of
          Nothing                -> return (pending, resumables)
          Just (e, mb_resumable) -> go (maybe id (:) mb_resumable resumables) (unwind throw e)
     | otherwise = return (pending, resumables)

-- TODO: currently I always unwind a pending exception. Similarly to above, I could choose not to unwind for a little bit
dequeueAsyncExceptionsOnBlocked :: Suspended s r -> ST s (Maybe [Resumable s r])
dequeueAsyncExceptionsOnBlocked (tid, masking, interrupter)
  | canThrow masking Interruptible = do
     mb_e <- dequeueAsyncException tid
     case mb_e of
       Nothing                -> return Nothing
       Just (e, mb_resumable) -> do
          throw <- interrupt interrupter
          (pending, resumables) <- dequeueAsyncExceptions tid (unwind throw e)
          return $ Just $ (tid, masking, throw, pending) : maybe id (:) mb_resumable resumables
  | otherwise = return Nothing

newtype Interrupter s r = Interrupter { interrupt :: ST s (Unwinder s r) } -- NB: must be used as a one-shot thing (once you grab the unwinder, the thread will be dumped from the suspended position and enqueued as pending by the user of Suspended)
type Suspended s r = (ThreadId s r, E.MaskingState, Interrupter s r)

data ThreadId s r = ThreadId Nat (STRef s (Queue (E.SomeException, STRef s (Maybe (ST s (Resumable s r))))))

instance Eq (ThreadId s r) where
    ThreadId n1 _ == ThreadId n2 _ = n1 == n2

instance Ord (ThreadId s r) where
    ThreadId n1 _ `compare` ThreadId n2 _ = n1 `compare` n2

instance Show (ThreadId s r) where
    show (ThreadId n _) = show n

instance Typeable (ThreadId s r) where
    typeOf _ = mkTyConApp (mkTyCon "ThreadId") [] -- TODO: update with correct FQN when I move the datatype

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


forkIO :: RTSM s r () -> RTSM s r (MC.ThreadId (RTSM s r))
forkIO forkable = RTSM $ \k tid masking suspendeds throw -> Pending $ \resumables next_tid scheduler -> newThreadId next_tid >>= \tid' -> scheduleM suspendeds ((tid, masking, throw, k tid') : (tid', masking, unhandledException, unRTSM forkable (\() -> Pending (scheduleM suspendeds)) tid' masking suspendeds unhandledException) : resumables) (next_tid + 1) scheduler

myThreadId :: RTSM s r (MC.ThreadId (RTSM s r))
myThreadId = RTSM $ \k tid  _masking _suspendeds _throw -> k tid


-- TODO: I could detect more unreachable states if I find that a RTSVar currently blocking a Pending gets GCed
data RTSVar s r a = RTSVar {
    rtsvar_data :: STRef s (Maybe a),
    -- MVars have guaranteed FIFO semantics, hence the queues
    rtsvar_putters :: STQ.STQueue s (STQ.Location s (Suspended s r), a, Resumable s r),
    rtsvar_takers  :: STQ.STQueue s (STQ.Location s (Suspended s r), a -> Resumable s r)
  }

newEmptyRTSVar :: RTSM s r (RTSVar s r a)
newEmptyRTSVar = newRTSVarInternal Nothing

newRTSVar :: a -> RTSM s r (RTSVar s r a)
newRTSVar = newRTSVarInternal . Just

newRTSVarInternal :: Maybe a -> RTSM s r (RTSVar s r a)
newRTSVarInternal mb_x = RTSM $ \k _tid _masking _suspendeds _throw -> Pending $ \resumables next_tid scheduler -> do
    data_ref <- newSTRef mb_x
    putter_queue <- STQ.new
    taker_queue <- STQ.new
    -- NB: unPending legitimate here because newRTSVarInternal cannot have externally visible side effects
    unPending (k (RTSVar data_ref putter_queue taker_queue)) resumables next_tid scheduler

takeRTSVar :: RTSVar s r a -> RTSM s r a
takeRTSVar rtsvar = RTSM $ \k tid masking suspendeds throw -> Pending $ \resumables next_tid scheduler -> do
    dat <- readSTRef (rtsvar_data rtsvar)
    case dat of
       -- NB: we must guarantee that the woken thread doing a putRTSVar (if any) completes its operation since takeMVar has this guarantee
      Just x  -> do
          (dat', resumables') <- STQ.dequeue (rtsvar_putters rtsvar) >>= \it -> case it of
              Nothing                         -> return (Nothing, resumables)
              Just (interrupt_loc, x, putter) -> STQ.delete interrupt_loc >>= \(Just _) -> return (Just x, putter : resumables)
          writeSTRef (rtsvar_data rtsvar) dat'
          scheduleM suspendeds ((tid, masking, throw, k x) : resumables') next_tid scheduler
      Nothing -> do
          rec { success_loc <- STQ.enqueue (interrupt_loc, \x -> (tid, masking, throw, k x)) (rtsvar_takers rtsvar)
                -- If we are interrupted, an asynchronous exception won the race: make sure that the standard wakeup loses
              ; interrupt_loc <- flip STQ.enqueue suspendeds (tid, masking, Interrupter $ STQ.delete success_loc >>= \(Just _) -> return throw)
              ; return () }
          scheduleM suspendeds resumables next_tid scheduler

putRTSVar :: RTSVar s r a -> a -> RTSM s r ()
putRTSVar rtsvar x = RTSM $ \k tid masking suspendeds throw -> Pending $ \resumables next_tid scheduler -> do
    dat <- readSTRef (rtsvar_data rtsvar)
    case dat of
       -- NB: we must guarantee that the woken thread doing a takeRTSVar (if any) completes its operation since putMVar has this guarantee
      Nothing -> do
          (dat', resumables') <- STQ.dequeue (rtsvar_takers rtsvar) >>= \it -> case it of
              Nothing                            -> return (Just x, resumables)
              Just (interrupt_loc, mk_resumable) -> STQ.delete interrupt_loc >>= \(Just _) -> return (Nothing, mk_resumable x : resumables)
          writeSTRef (rtsvar_data rtsvar) dat'
          scheduleM suspendeds ((tid, masking, throw, k ()) : resumables') next_tid scheduler
      Just x  -> do
          rec { success_loc <- STQ.enqueue (interrupt_loc, x, (tid, masking, throw, k ())) (rtsvar_putters rtsvar)
                -- If we are interrupted, an asynchronous exception won the race: make sure that the standard wakeup loses
              ; interrupt_loc <- flip STQ.enqueue suspendeds (tid, masking, Interrupter $ STQ.delete success_loc >>= \(Just _) -> return throw)
              ; return () }
          scheduleM suspendeds resumables next_tid scheduler


example1 :: RTSM s r Integer
example1 = do
    yield
    v <- newEmptyRTSVar
    --putRTSVar v 3
    putRTSVar v 3
    yield
    --takeRTSVar v
    takeRTSVar v

example2 :: RTSM s r Integer
example2 = do
    v_in <- newRTSVar 1336
    v_out <- newEmptyRTSVar
    _ <- forkIO $ do
        x <- takeRTSVar v_in
        yield
        putRTSVar v_out (x + 1)
    takeRTSVar v_out

-- An example with a race: depending on scheduling, this either returns "Hello" or "World"
example3 :: RTSM s r String
example3 = do
    v <- newEmptyRTSVar
    _ <- forkIO $ putRTSVar v "Hello"
    _ <- forkIO $ putRTSVar v "World"
    takeRTSVar v


testScheduleSafe :: Eq r => (forall s. RTSM s r r) -> IO ()
-- Cuter:
--testScheduleSafe act = test $ \sched -> expected == runRTSM sched act
-- More flexible:
testScheduleSafe act = test $ \ss -> trace (show ss) $ expected == runRTSM (schedulerStreamed ss) act
-- Working:
--testScheduleSafe act = quickCheck $ \gen -> expected == runRTSM (randomised gen) act
  where expected = runRTSM roundRobin act


main :: IO ()
main = do
    -- Demonstrates the presence of a data race - these two invocations return different results
    print $ runRTSM unfair example3
    print $ runRTSM roundRobin example3

    -- Let's see if we can find the race automatically!
    testScheduleSafe example3

