{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, TypeFamilies, DeriveDataTypeable, StandaloneDeriving, ExistentialQuantification #-}
module Control.Monad.RTS (
    Scheduler, unfair, roundRobin, streamed,
    RTS, runRTS,
    testScheduleSafe
  ) where

-- Ideas:
--  * It might be cool to have a mode that generates random asynchronous exceptions to try to crash other threads
--  * We have to be able to show failing schedulings in a nice way
--  * I could detect more unreachable states if I find that a MVar currently blocking a Pending gets GCed


import Control.Applicative (Applicative(..))
import Control.Arrow ((***))
import qualified Control.Exception as E
import Control.Monad
import qualified Control.Monad.Concurrent as MC
import Control.Monad.Fix
import Control.Monad.ST
import Control.Monad.ST.Class

import Data.Foldable (Foldable(foldMap))
import Data.List
import Data.Maybe (maybeToList)
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
    show _ = "Scheduler"

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


-- | A closure of a value from the user over the set of SyncObjects that it could possibly reference
type Closure s r a = (SetEq (SyncObject s r), a)


newtype SetEq a = SetEq { unSetEq :: [a] }

instance Eq a => Eq (SetEq a) where
    se1 == se2 = all (`elem` unSetEq se2) (unSetEq se1) -- This is correct because elements in both sets must be unique

emptySetEq :: SetEq a
emptySetEq = SetEq []

singletonSetEq :: a -> SetEq a
singletonSetEq x = SetEq [x]

insertSetEq :: Eq a => a -> SetEq a -> SetEq a
insertSetEq x se = if x `elem` unSetEq se then se else SetEq $ x : unSetEq se

unionSetEq :: Eq a => SetEq a -> SetEq a -> SetEq a
unionSetEq se1 se2 = SetEq $ nub $ unSetEq se1 ++ unSetEq se2

instance Eq a => Monoid (SetEq a) where
    mempty = emptySetEq
    mappend = unionSetEq


-- | Both blocked and unblocked threads have some information in common.
type Thread s r = (ThreadId s r,
                   Unwinder s r)

-- | Unblocked threads are those that are available for immediate execution. There is no immediate
-- problem preventing them from making progress.
--
-- These threads will either take delivery of an asynchronous exception or continue normally when rescheduled.
type Unblocked s r = (Thread s r, Pending s r)

-- | Blocked threads are those that cannot currently be executed because they are waiting for another
-- thread to get back to them. In this case, the corresponding 'Pending' action is stored in the corresponding
-- synchronisation object (i.e. the MVar or ThreadId blocked on).
--
-- These threads will either take delivery of an asynchronous exception or continue normally after both the
-- other thread has got back to them and they get rescheduled.
type Blocked s r = Thread s r

-- | A pending coroutine
--
-- You almost always want to use (scheduleM ((tid, throw, k) : unblockeds)) rather than (unPending k unblockeds) because
-- scheduleM gives QuickCheck a chance to try other schedulings, whereas using unPending forces control
-- flow to continue in the current thread.
type Pending s r = Pending' s r (Result r)

-- | Generalised pending corountine (a reader monad)
newtype Pending' s r a = Pending { unPending :: ([(Maybe (SyncObject s r), Unblocked s r)] -> Nat -> ST s (Result r)) -- ^ Rescheduling continuation: used whenever we are about to block on something (allow Nothing SyncObject for a yield call)
                                             -> Nat                                                                   -- ^ Next ThreadId to allocate (NB: could extract reader monad structure w/ the continuation above?)
                                             -> ST s a }

instance Functor (Pending' s r) where
    fmap = liftM

instance Applicative (Pending' s r) where
    pure = return
    (<*>) = ap

instance Monad (Pending' s r) where
    return x = liftST (return x)
    mx >>= fxmy = Pending $ \k_schedule next_tid -> unPending mx k_schedule next_tid >>= \x -> unPending (fxmy x) k_schedule next_tid

reschedule :: [(Maybe (SyncObject s r), Unblocked s r)] -> Pending s r
reschedule unblockeds = Pending $ \k_schedule next_tid -> k_schedule unblockeds next_tid

instance MonadST (Pending' s r) where
    type StateThread (Pending' s r) = s
    liftST st = Pending $ \_k_schedule _next_tid -> st


newtype RTS s r a = RTS { unRTS :: (Closure s r a -> Pending s r) -- ^ Continuation: how we should continue after we have our result
                                -> STQ.STQueue s (Blocked s r)    -- ^ Blocked threads that may or may not have been resumed yet: this is necessary because we may want to deliver asyncronous exceptions to them. As such, everything in this list is Interruptible.
                                -> Closure s r (Thread s r)
                                -> Pending s r }
-- | We have to be able to throw several exceptions in succession because we can have more than one pending asynchronous exceptions.
data Unwinder s r = Unwinder {
    -- | Where we stand wrt. asynchronous exceptions: this is used to control whether we can actually unwind
    masking         :: E.MaskingState,
    -- | The ST action be used as a one-shot thing. For blocked threads, once you run the ST action (to deliver an asynchronous exception), the thread
    -- will be dumped from the suspended position and enqueued as pending by the user of Blocked
    uncheckedUnwind :: Closure s r E.SomeException
                    -> ST s (ThreadId s r -> Pending s r) -- After unwinding we actually always resume on the same thread -- the ThreadId business is a nice hack to reuse Unblocked
  }

maskUnwinder :: Unwinder s r -> Interruptibility -> Unwinder s r
maskUnwinder throw Interruptible   = throw { masking = case masking throw of E.MaskedUninterruptible -> E.MaskedUninterruptible; _ -> E.MaskedInterruptible }
maskUnwinder throw Uninterruptible = throw { masking = E.MaskedUninterruptible }


unwindAsync :: Thread s r -> Interruptibility -> Maybe (Closure s r E.SomeException -> ST s (Pending s r))
unwindAsync (tid, throw) interruptible = guard (canThrow (masking throw) interruptible) >> return (fmap ($ tid) . uncheckedUnwind throw)

unwindSync :: Thread s r -> Closure s r E.SomeException -> Pending s r
unwindSync (tid, throw) clo_e = join $ liftST $ fmap ($ tid) (uncheckedUnwind throw clo_e)

runRTS :: Scheduler -> (forall s. RTS s r r) -> Result r
runRTS scheduler mx = runST $ do
    tid <- newThreadId 0
    blockeds <- STQ.new
    unPending (unRTS mx (\(_syncobjs, x) -> return (Success x)) blockeds (singletonSetEq (SyncThreadId tid), (tid, unhandledException E.Unmasked))) (scheduleM scheduler blockeds) 1

unhandledException :: E.MaskingState -> Unwinder s r
unhandledException masking = Unwinder {
    masking = masking,
    uncheckedUnwind = \(_syncobjs, e) -> return (\_tid -> return (UnhandledException e)) -- We only report the last unhandled exception. Could we do something else?
  }


instance Functor (RTS s r) where
    fmap = liftM

instance Applicative (RTS s r) where
   pure = return
   -- We can be more precise about how syncobjs flow for an Applicative computation than if we just used `ap` directly. This helps trim the search space:
   mfxy <*> mx = RTS $ \k_y blockeds (syncobjs, (tid, throw)) -> unRTS mfxy (\(syncobjs_fxy, fxy) -> unRTS mx (\(syncobjs_x, x) -> k_y (syncobjs_fxy `unionSetEq` syncobjs_x, fxy x)) blockeds (syncobjs, (tid, throw))) blockeds (syncobjs, (tid, throw))

instance Monad (RTS s r) where
    return x = RTS $ \k _blockeds (syncobjs, _thread) -> k (syncobjs, x)
    mx >>= fxmy = RTS $ \k_y blockeds (syncobjs, (tid, throw)) -> unRTS mx (\(syncobjs', x) -> unRTS (fxmy x) k_y blockeds (syncobjs', (tid, throw))) blockeds (syncobjs, (tid, throw))

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
getMaskingState = RTS $ \k _blockeds (syncobjs, (_tid, throw)) -> k (syncobjs, masking throw)

maskWith :: Interruptibility -> ((forall a. RTS s r a -> RTS s r a) -> RTS s r b) -> RTS s r b
maskWith interruptible while = RTS $ \k blockeds (syncobjs, (tid, throw)) -> reschedule [(Just (SyncThreadId tid), ((tid, throw), unRTS (while (\unmask -> RTS $ \k' blockeds' (syncobjs', (tid', throw')) -> unRTS unmask k' blockeds' (syncobjs', (tid', throw' { masking = masking throw })))) (\b -> prepare [k b]) blockeds (syncobjs, (tid, throw `maskUnwinder` interruptible))))]

throwIO :: E.Exception e => e -> RTS s r a
throwIO e = RTS $ \_k _blockeds (syncobjs, thread) -> unwindSync thread (syncobjs, E.SomeException e)

throwTo :: E.Exception e => ThreadId s r -> e -> RTS s r ()
throwTo target_tid e = RTS $ \k blockeds (syncobjs, thread@(tid, throw)) -> case target_tid == tid of
    True -> unwindSync thread (syncobjs, E.SomeException e) -- See GHC #4888: we always throw an exception regardless of the mask mode
    False -> reschedule [(Just (SyncThreadId target_tid), (thread, do
      -- If we ourselves get interrupted by an asynchronous exception before the one we sent was delivered,
      -- recover by still delivering the exception but ensure that doing so does not cause the pending list to change
      _ <- liftST $ mfix $ \kill_interruptable -> do
        kill_blocked <- enqueueAsyncException target_tid (syncobjs, E.SomeException e) (thread, k (syncobjs, ())) kill_interruptable
        blocked_loc <- flip STQ.enqueue blockeds (tid, throw { uncheckedUnwind = \e -> kill_blocked >> uncheckedUnwind throw e })
        return $ STQ.delete blocked_loc >>= \(Just _) -> return ()
      reschedule []))]

catch :: E.Exception e => RTS s r a -> (e -> RTS s r a) -> RTS s r a
catch mx handle = RTS $ \k blockeds (syncobjs, (tid, throw)) -> unRTS mx k blockeds (syncobjs, (tid, throw { uncheckedUnwind = \(syncobjs', e) -> maybe (uncheckedUnwind throw (syncobjs', e)) (\e -> return (\tid -> unRTS (handle e) k blockeds (syncobjs `mappend` syncobjs', (tid, throw)))) (E.fromException e) }))

-- | Give up control to the scheduler. Control is automatically given up to the scheduler after calling every RTS primitive
-- which might have effects observable outside the current thread. This is enough to almost guarantee that there exists some
-- scheduler for which the RTS monad will give the same results as the IO monad.
--
-- The exception to this guarantee is if you write a non-terminating computation on a thread (other than the initial thread) which
-- does not call any RTS primitive that gives up control to the scheduler. For such computations, you need to manually add a
-- call to 'yield' to allow the scheduler to interrupt the loop.
yield :: RTS s r ()
yield = RTS $ \k _blockeds (syncobjs, thread) -> reschedule [(Nothing, (thread, k (syncobjs, ())))]
  -- It is certainly enough to yield on every bind operation. But this is too much (and it breaks the monad laws).
  -- Quviq/PULSE yields just before every side-effecting operation. I think we can just yield after every side-effecting
  -- operation and get the same results.


scheduleM :: Scheduler -> STQ.STQueue s (Blocked s r) -> [(Maybe (SyncObject s r), Unblocked s r)] -> Nat -> ST s (Result r)
scheduleM scheduler blockeds unblockeds next_tid = do
    -- 1) We could continue by just stepping the unblockeds to their next yield point
    let possibilities0 = [return (pending, unblockeds') | ((_syncobject, (_thread, pending)), unblockeds') <- holes unblockeds]
    -- 2) We could continue by delivering asynchronous exceptions to the unblocked threads
    possibilities1 <- flip mapMaybeM (holes unblockeds) $ \((_syncobject, (thread, _pending)), unblockeds') -> fmap (fmap (fmap (\possibility -> (possibility, unblockeds')))) $ dequeueAsyncException' Uninterruptible thread
    -- 3) We could continue by delivering asynchronous exceptions to the blocked threads
    -- This is the only mechanism that lets such threads wake up, bar the blocking call resuming normally.
    current_blockeds <- STQ.toList blockeds
    possibilities2 <- flip mapMaybeM current_blockeds $ \thread -> fmap (fmap (fmap (\possibility -> (possibility, unblockeds)))) $ dequeueAsyncException' Interruptible thread
      -- Note that the resumed thing will take care of deleting this blocked entery from the queue
    
    -- Use a scheduling strategy to decide which of these possibilities we should try out:
    case possibilities0 ++ possibilities1 ++ possibilities2 of
      [] -> return BlockedIndefinitely
      possibilities -> do
          let (scheduler', i) = schedule scheduler (genericLength possibilities - 1)
          (pending, unblockeds') <- possibilities `genericIndex` i
          unPending pending (\unblockeds'' -> scheduleM scheduler' blockeds (unblockeds' ++ unblockeds'')) next_tid


instance MonadST (RTS s r) where
    type StateThread (RTS s r) = s
    liftST st = RTS $ \k _blockeds (syncobjs, _thread) -> Pending $ \k_schedule next_tid -> st >>= \x -> unPending (k (syncobjs, x)) k_schedule next_tid

canThrow :: E.MaskingState -> Interruptibility -> Bool
canThrow E.Unmasked            _             = True
canThrow E.MaskedInterruptible Interruptible = True
canThrow _                     _             = False

dequeueAsyncException' :: Interruptibility -> Thread s r -> ST s (Maybe (ST s (Pending s r)))
dequeueAsyncException' interruptible thread@(tid, _) = do
    case unwindAsync thread interruptible of
       -- Cannot unwind this (blocked) thread right now due to masking
      Nothing -> return Nothing
      Just unchecked_unwind -> do
        no_exceptions <- nullAsyncExceptions tid
        if no_exceptions
         then return Nothing
         else return $ Just $ do
          Just (e, mb_resumable) <- dequeueAsyncException tid
          pending <- unchecked_unwind e
          return $ prepare (maybe [pending] (\(_, resumed_pending) -> resumed_pending : [pending]) mb_resumable)

data ThreadId s r = ThreadId Nat (STRef s (Queue (Closure s r E.SomeException, STRef s (Maybe (ST s (Unblocked s r))))))

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

enqueueAsyncException :: ThreadId s r -> Closure s r E.SomeException -> Unblocked s r -> ST s () -> ST s (ST s ())
enqueueAsyncException (ThreadId _ ref) e resumable notify_block_complete = do
    asyncs <- readSTRef ref
    resumable_ref <- newSTRef $ Just $ notify_block_complete >> return resumable
    writeSTRef ref (queue (e, resumable_ref) asyncs)
    return $ readSTRef resumable_ref >>= \(Just _) -> writeSTRef resumable_ref Nothing

nullAsyncExceptions :: ThreadId s r -> ST s Bool
nullAsyncExceptions (ThreadId _ ref) = fmap nullQueue (readSTRef ref)

dequeueAsyncException :: ThreadId s r -> ST s (Maybe (Closure s r E.SomeException, Maybe (Unblocked s r)))
dequeueAsyncException (ThreadId _ ref) = do
    asyncs <- readSTRef ref
    case dequeue asyncs of
      Nothing                            -> return Nothing
      Just ((e, resumable_ref), asyncs') -> writeSTRef ref asyncs' >> readSTRef resumable_ref >>= \mb_get_resumable -> Traversable.sequence mb_get_resumable >>= \mb_resumable -> return (Just (e, mb_resumable))


forkIO :: RTS s r () -> RTS s r (MC.ThreadId (RTS s r))
forkIO forkable = RTS $ \k blockeds (syncobjs, (_, Unwinder { masking = masking })) -> Pending $ \k_schedule next_tid -> do
    tid' <- newThreadId next_tid
    let syncobjs' = SyncThreadId tid' `insertSetEq` syncobjs
    unPending (prepare [k (syncobjs', tid'), unRTS forkable (\(_syncobjs, ()) -> Pending $ \k_schedule -> k_schedule []) blockeds (syncobjs', (tid', unhandledException masking))]) k_schedule (next_tid + 1)

myThreadId :: RTS s r (MC.ThreadId (RTS s r))
myThreadId = RTS $ \k _blockeds (syncobjs, (tid, _throw)) -> k (syncobjs, tid)


instance MC.MonadMVar (RTS s r) where
    type MC.MVar (RTS s r) = MVar s r
    
    newEmptyMVar = newEmptyMVar
    newMVar = newMVar
    
    takeMVar = takeMVar
    putMVar = putMVar
    
    tryTakeMVar = undefined
    tryPutMVar = undefined
    isEmptyMVar = undefined


data MVar s r a = MVar {
    mvar_data :: STRef s (Maybe a),
    -- MVars have guaranteed FIFO semantics, hence the queues
    mvar_putters :: STQ.STQueue s (                 ST s (a, Pending s r)),
    mvar_takers  :: STQ.STQueue s (Closure s r a -> ST s (Pending s r))
  }

deriving instance Eq (MVar s r a)

instance Typeable1 (MVar s r) where
    typeOf1 _ = mkTyConApp (mkTyCon "Control.Monad.RTS.MVar") []

newEmptyMVar :: RTS s r (MVar s r a)
newEmptyMVar = newMVarInternal Nothing

newMVar :: a -> RTS s r (MVar s r a)
newMVar = newMVarInternal . Just

newMVarInternal :: Maybe a -> RTS s r (MVar s r a)
newMVarInternal mb_x = RTS $ \k _blockeds (syncobjs, _thread) -> do
    mvar <- liftST $ do
        data_ref <- newSTRef mb_x
        putter_queue <- STQ.new
        taker_queue <- STQ.new
        return (MVar data_ref putter_queue taker_queue)
    -- NB: unPending legitimate here because newMVarInternal cannot have externally visible side effects
    k (SyncMVar mvar `insertSetEq` syncobjs, mvar)

takeMVar :: MVar s r a -> RTS s r a
takeMVar mvar = RTS $ \k blockeds (syncobjs, thread@(tid, throw)) -> reschedule [(Just (SyncMVar mvar), (thread, Pending $ \k_schedule next_tid -> do
    dat <- readSTRef (mvar_data mvar)
    case dat of
       -- NB: we must guarantee that the woken thread doing a putMVar (if any) completes its operation since takeMVar has this guarantee
      Just x  -> do
          (dat', mb_unblocked) <- STQ.dequeue (mvar_putters mvar) >>= \it -> case it of
              Nothing            -> return (Nothing, Nothing)
              Just interrupt_act -> fmap (Just *** Just) interrupt_act
          writeSTRef (mvar_data mvar) dat'
          unPending (prepare (k (syncobjs, x) : maybeToList mb_unblocked)) k_schedule next_tid
      Nothing -> do
          _ <- mfix $ \interrupt_act -> do
              success_loc <- STQ.enqueue interrupt_act (mvar_takers mvar)
              -- If we are interrupted, an asynchronous exception won the race: make sure that the standard wakeup loses
              interrupt_loc <- flip STQ.enqueue blockeds (tid, throw { uncheckedUnwind = \e -> STQ.delete success_loc >>= \(Just _) -> uncheckedUnwind throw e })
              return $ \(syncobjs', x) -> STQ.delete interrupt_loc >>= \(Just _) -> return (k (syncobjs `mappend` syncobjs', x))
          k_schedule [] next_tid))]

putMVar :: MVar s r a -> a -> RTS s r ()
putMVar mvar x = RTS $ \k blockeds (syncobjs, thread@(tid, throw)) -> reschedule [(Just (SyncMVar mvar), (thread, Pending $ \k_schedule next_tid -> do
    dat <- readSTRef (mvar_data mvar)
    case dat of
       -- NB: we must guarantee that the woken thread doing a takeMVar (if any) completes its operation since putMVar has this guarantee
      Nothing -> do
          (dat', mb_unblocked) <- STQ.dequeue (mvar_takers mvar) >>= \it -> case it of
              Nothing            -> return (Just x, Nothing)
              Just interrupt_act -> fmap (((,) Nothing) . Just) (interrupt_act (syncobjs, x))
          writeSTRef (mvar_data mvar) dat'
          unPending (prepare (k (syncobjs, ()) : maybeToList mb_unblocked)) k_schedule next_tid
      Just x  -> do
          _ <- mfix $ \interrupt_act -> do
              success_loc <- STQ.enqueue interrupt_act (mvar_putters mvar)
              -- If we are interrupted, an asynchronous exception won the race: make sure that the standard wakeup loses
              interrupt_loc <- flip STQ.enqueue blockeds (tid, throw { uncheckedUnwind = \e -> STQ.delete success_loc >>= \(Just _) -> uncheckedUnwind throw e })
              return $ STQ.delete interrupt_loc >>= \(Just _) -> return (x, (k (syncobjs, ())))
          k_schedule [] next_tid))]


prepare :: [Pending s r] -> Pending s r
prepare = go []
  where
    go syncobjs_unblockeds []             = Pending $ \k_schedule next_tid -> k_schedule syncobjs_unblockeds next_tid
    go syncobjs_unblockeds (pending:rest) = Pending $ \k_schedule next_tid -> unPending pending (\syncobjs_unblockeds' next_tid' -> unPending (go (syncobjs_unblockeds' ++ syncobjs_unblockeds) rest) k_schedule next_tid') next_tid


_example1 :: RTS s r Integer
_example1 = do
    yield
    v <- newEmptyMVar
    --putMVar v 3
    putMVar v 3
    yield
    --takeMVar v
    takeMVar v

_example2 :: RTS s r Integer
_example2 = do
    v_in <- newMVar 1336
    v_out <- newEmptyMVar
    _ <- forkIO $ do
        x <- takeMVar v_in
        yield
        putMVar v_out (x + 1)
    takeMVar v_out

-- An example with a race: depending on scheduling, this either returns "Hello" or "World"
_example3 :: RTS s r String
_example3 = do
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


_main :: IO ()
_main = do
    -- Demonstrates the presence of a data race - these two invocations return different results
    print $ runRTS unfair _example3
    print $ runRTS roundRobin _example3

    -- Let's see if we can find the race automatically!
    testScheduleSafe _example3

