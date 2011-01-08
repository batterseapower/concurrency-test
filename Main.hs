{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, TypeFamilies #-}

import Control.Arrow ((***), first, second)
import Control.Applicative (Applicative(..))
import Control.Monad
import qualified Control.Exception as E

import Control.Monad.ST
import Data.STRef
import Data.List
import Data.Monoid (mempty)
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))
import Data.Typeable (Typeable(..), mkTyCon, mkTyConApp)
import qualified Data.Map as M

import Test.LazySmallCheck
import Test.QuickCheck hiding (Success, Result, (><))
import Test.QuickCheck.Gen

import System.Random

import Prelude hiding (catch)


import qualified Control.Monad.Concurrent as MC


import Debug.Trace

import System.IO.Unsafe


instance Applicative (ST s) where
    pure = return
    (<*>) = ap

instance Arbitrary StdGen where
    arbitrary = MkGen $ \gen _ -> gen
    shrink _ = []


{-# NOINLINE exceptionTrace #-}
exceptionTrace :: a -> a
exceptionTrace x = unsafePerformIO (E.evaluate x `E.catch` (\e -> trace ("Exception in pure code: " ++ show (e :: E.SomeException)) $ E.throw e))

-- I used to use unsafeIsEvaluated to decide where to put in "...", but that pruned too heavily because
-- I couldn't show the schedule before it was actually poked on and those thunks turned into real values.
{-# NOINLINE showsExplored #-}
showsExplored :: (a -> ShowS) -> a -> ShowS
showsExplored shows x = unsafePerformIO $ fmap (maybe (showString "...") shows) $ tryIf isLSCError (E.evaluate x)
  where
    -- Looked at the LSC code to see what sort of errors it was generating...
    isLSCError (E.ErrorCall ('\0':msg)) = True
    isLSCError _                        = False
    
    tryIf :: E.Exception e => (e -> Bool) -> IO a -> IO (Maybe a)
    tryIf p act = fmap (either (\() -> Nothing) Just) $ E.tryJust (\e -> guard (p e) >> return ()) act


newtype Nat = Nat { unNat :: Int }
            deriving (Eq, Ord)

instance Show Nat where
    show = show . unNat

instance Serial Nat where
    series d = drawnFrom $ map Nat [0..d]

instance Num Nat where
    x + y = Nat (unNat x + unNat y)
    x * y = Nat (unNat x * unNat y)
    x - y | z < 0     = error $ "Subtracting the naturals " ++ show x ++ " and " ++ show y ++ " produced a negative answer"
          | otherwise = Nat z
      where z = unNat x - unNat y
    negate (Nat 0) = Nat 0
    negate x = error $ "Cannot negate the strictly-positive natural number " ++ show x
    abs x = x
    signum (Nat 0) = Nat 0
    signum (Nat x) = Nat 1
    fromInteger x | x < 0     = error $ "The integer " ++ show x ++ " was not a natural number"
                  | otherwise = Nat (fromInteger x)

instance Real Nat where
    toRational = toRational . unNat

instance Enum Nat where
    succ x = Nat (succ (unNat x))
    pred (Nat 0) = error "Cannot take the predecessor of the natural number 0"
    pred x       = Nat (pred (unNat x))
    toEnum x | x < 0     = error $ "Invalid argument to toEnum: " ++ show x
             | otherwise = Nat x
    fromEnum = unNat

instance Integral Nat where
    x `quot` y = Nat (unNat x `quot` unNat y)
    x `rem` y = Nat (unNat x `rem` unNat y)
    x `div` y = Nat (unNat x `div` unNat y)
    x `mod` y = Nat (unNat x `mod` unNat y)
    x `quotRem` y = (Nat *** Nat) (unNat x `quotRem` unNat y)
    x `divMod` y = (Nat *** Nat) (unNat x `divMod` unNat y)
    toInteger = toInteger . unNat

instance Random Nat where
    randomR (lo, hi) g = first Nat $ randomR (unNat lo, unNat hi) g
    random g = first (Nat . abs) $ random g


data Queue a = Queue [a] [a]

emptyQueue :: Queue a
emptyQueue = Queue [] []

queue :: a -> Queue a -> Queue a
queue x (Queue xs ys) = Queue (x : xs) ys

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue xs     (y:ys)) = Just (y, Queue xs ys)
dequeue (Queue []     [])     = Nothing
dequeue (Queue (x:xs) [])     = Just (rev xs x [])
  where
    rev []     x acc = (x, Queue [] acc)
    rev (y:ys) x acc = rev ys y (x:acc)


data Stream a = a :< Stream a

instance Show a => Show (Stream a) where
    showsPrec d = showsExplored (\(x :< xs) -> showParen (d > 0) $ showsPrec 1 x . showString " :< " . showsPrec 1 xs)

instance Serial a => Serial (Stream a) where
    series = cons2 (:<)

genericIndexStream :: Num i => Stream a -> i -> a
genericIndexStream (x :< xs) n = if n == 0 then x else genericIndexStream xs (n - 1)


data Streem a = Streem a (Stream (Streem a))
              deriving (Show)

instance Serial a => Serial (Streem a) where
    series = cons2 Streem . (+1)


-- | A stream suitable for use for guiding the scheduler. The natural number stored in the nth element
-- of one of the Stream (Streem Nat) we contain is drawn uniformly from the range [0,n].
--
-- In one use of the scheduler, all but one element of each Stream will be discarded, since they correspond
-- to schedulings for executions with more or less pending processes than we actually saw
newtype SchedulerStreem = SS { unSS :: Stream (Streem Nat) }
                        deriving (Show)

instance Serial SchedulerStreem where
    series = cons SS >< streamSeries . (+1)
      where
        streemSeries :: Nat -> Series (Streem Nat)
        streemSeries n = (cons Streem >< (\_ -> drawnFrom [0..n]) >< streamSeries) . (+1)
    
        streamSeries :: Series (Stream (Streem Nat))
        streamSeries = streamSeries' 0
    
        streamSeries' :: Nat -> Series (Stream (Streem Nat))
        streamSeries' n = cons (:<) >< streemSeries n >< streamSeries' (n + 1)


genericDeleteAt :: Num i => [a] -> i -> (a, [a])
genericDeleteAt []     _ = error $ "genericDeleteAt: index too large for given list, or list null"
genericDeleteAt (x:xs) n = if n == 0 then (x, xs) else second (x:) (genericDeleteAt xs (n - 1))


newtype Scheduler = Scheduler { schedule :: forall s r.    Nat              -- ^ One less than the number of pending processes (n)
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

schedulerStreemed :: SchedulerStreem -> Scheduler
schedulerStreemed (SS sss) = Scheduler schedule
  where schedule n = (schedulerStreemed (SS sss'), i)
          where Streem i sss' = genericIndexStream sss n

randomised :: StdGen -> Scheduler
randomised gen = Scheduler schedule
  where schedule n = (randomised gen', i)
          where (i, gen') = randomR (0, n) gen

instance Show Scheduler where
    show _ = "Scheduler" -- FIXME: have to be able to show failing schedulings in a nice way

instance Arbitrary Scheduler where
    arbitrary = fmap randomised arbitrary
    shrink _ = []

instance Serial Scheduler where
    series = cons schedulerStreemed >< series

-- FIXME: think about what happens if we get something other than Success on a non-main thread.
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
-- You almost always want to use (scheduleM (k : pendings)) rather than (unPending k pendings) because
-- scheduleM gives QuickCheck a chance to try other schedulings, whereas using unPending forces control
-- flow to continue in the current thread.
newtype Pending s r = Pending { unPending :: [Pending s r] -- ^ Runnable threads that are eligible for execution
                                          -> Nat           -- ^ Next ThreadId to allocate
                                          -> Scheduler     -- ^ Current scheduler, used for at the next rescheduling point
                                          -> ST s (Result r) }
newtype RTSM s r a = RTSM { unRTSM :: (a -> Pending s r)               -- ^ Continuation: how we should continue after we have our result
                                   -> ThreadId s r                     -- ^ ThreadId of this thread of execution
                                   -> (E.SomeException -> Pending s r) -- ^ Exception handling continuation
                                   -> Pending s r }

runRTSM :: Scheduler -> (forall s. RTSM s r r) -> Result r
runRTSM scheduler mx = runST (newThreadId 0 >>= \tid -> unPending (unRTSM mx (\x -> Pending $ \_pendings _next_tid _scheduler -> return (Success x)) tid unhandledException) [] 1 scheduler)

unhandledException :: E.SomeException -> Pending s r
unhandledException e = Pending $ \_ _ _ -> return (UnhandledException e)


instance Functor (RTSM s r) where
    fmap = liftM

instance Applicative (RTSM s r) where
   pure = return
   (<*>) = ap

instance Monad (RTSM s r) where
    return x = RTSM $ \k _tid _throw -> k x
    mx >>= fxmy = RTSM $ \k_y tid throw -> unRTSM mx (\x -> unRTSM (fxmy x) k_y tid throw) tid throw

instance MC.MonadException (RTSM s r) where
    mask = mask
    
    throwIO = throwIO
    throwTo = throwTo
    catch = catch

instance MC.MonadConcurrent (RTSM s r) where
    type MC.ThreadId (RTSM s r) = ThreadId s r

    forkIO = forkIO
    myThreadId = myThreadId

    yield = yield


mask :: ((forall a. RTSM s r a -> RTSM s r a) -> RTSM s r b) -> RTSM s r b
mask while = RTSM $ \k tid throw -> undefined -- FIXME: must consider mask mode in throwTo!

throwIO :: E.Exception e => e -> RTSM s r a
throwIO e = RTSM $ \_k _tid throw -> throw (E.SomeException e)

throwTo :: E.Exception e => ThreadId s r -> e -> RTSM s r ()
throwTo target_tid e = RTSM $ \k tid throw -> if target_tid == tid then throw (E.SomeException e) else undefined -- FIXME

catch :: E.Exception e => RTSM s r a -> (e -> RTSM s r a) -> RTSM s r a
catch mx handle = RTSM $ \k tid throw -> unRTSM mx k tid $ \e -> maybe (throw e) (\e -> unRTSM (handle e) k tid throw) (E.fromException e)

-- | Give up control to the scheduler: yields should be used at every point where it is useful to allow QuickCheck
-- to try several different scheduling options.
--
-- It is certainly enough to yield on every bind operation. But this is too much (and it breaks the monad laws).
-- Quviq/PULSE yields just before every side-effecting operation. I think we can just yield after every side-effecting
-- operation and get the same results.
yield :: RTSM s r ()
yield = RTSM $ \k _tid _throw -> Pending $ \pendings -> scheduleM (k () : pendings)

scheduleM :: [Pending s r] -> Nat -> Scheduler -> ST s (Result r)
scheduleM pendings next_tid scheduler = case pendings of
    [] -> return BlockedIndefinitely
    _  -> unPending k pendings' next_tid scheduler'
      where (scheduler', i) = schedule scheduler (genericLength pendings - 1)
            (k, pendings') = genericDeleteAt pendings i


data ThreadId s r = ThreadId Nat (STRef s (Queue (E.SomeException, Pending s r)))

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


forkIO :: RTSM s r () -> RTSM s r (MC.ThreadId (RTSM s r))
forkIO forkable = RTSM $ \k _tid _throw -> Pending $ \pendings next_tid scheduler -> newThreadId next_tid >>= \tid' -> scheduleM (k tid' : unRTSM forkable (\() -> Pending scheduleM) tid' unhandledException : pendings) (next_tid + 1) scheduler

myThreadId :: RTSM s r (MC.ThreadId (RTSM s r))
myThreadId = RTSM $ \k tid _throw -> k tid


data RTSVarState s r a = RTSVarState {
    rtsvar_data :: Maybe a,
    -- MVars have guaranteed FIFO semantics, so that's what we do here
    rtsvar_putters :: Queue (a, Pending s r),
    rtsvar_takers :: Queue (a -> Pending s r)
  }

newtype RTSVar s r a = RTSVar { unRTSVar :: STRef s (RTSVarState s r a) } -- TODO: I could detect more unreachable states if I find that a RTSVar currently blocking a Pending gets GCed

newEmptyRTSVar :: RTSM s r (RTSVar s r a)
newEmptyRTSVar = newRTSVarInternal Nothing

newRTSVar :: a -> RTSM s r (RTSVar s r a)
newRTSVar = newRTSVarInternal . Just

newRTSVarInternal :: Maybe a -> RTSM s r (RTSVar s r a)
newRTSVarInternal mb_x = RTSM $ \k _tid _throw -> Pending $ \pendings next_tid scheduler -> newSTRef (RTSVarState mb_x emptyQueue emptyQueue) >>= \stref -> unPending (k (RTSVar stref)) pendings next_tid scheduler -- NB: unPending legitimate here because newRTSVarInternal cannot have externally visible side effects

takeRTSVar :: RTSVar s r a -> RTSM s r a
takeRTSVar rtsvar = RTSM $ \k _tid _throw -> Pending $ \pendings next_tid scheduler -> do
    state <- readSTRef (unRTSVar rtsvar)
    case rtsvar_data state of
       -- NB: we must guarantee that the woken thread doing a putRTSVar (if any) completes its operation since takeMVar has this guarantee
      Just x  -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_data = dat', rtsvar_putters = putters' }) >> scheduleM (k x : pendings') next_tid scheduler
        where (dat', putters', pendings') = case dequeue (rtsvar_putters state) of
                  Nothing                      -> (Nothing, emptyQueue, pendings)
                  Just ((x, putter), putters') -> (Just x, putters', putter : pendings)
      Nothing -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_takers = queue k (rtsvar_takers state) }) >> scheduleM pendings next_tid scheduler

putRTSVar :: RTSVar s r a -> a -> RTSM s r ()
putRTSVar rtsvar x = RTSM $ \k _tid _throw -> Pending $ \pendings next_tid scheduler -> do
    state <- readSTRef (unRTSVar rtsvar)
    case rtsvar_data state of
       -- NB: we must guarantee that the woken thread doing a takeRTSVar (if any) completes its operation since putMVar has this guarantee
      Nothing -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_data = dat', rtsvar_takers = takers' }) >> scheduleM (k () : pendings') next_tid scheduler
        where (dat', takers', pendings') = case dequeue (rtsvar_takers state) of
                  Nothing                     -> (Just x, emptyQueue, pendings)
                  Just (mk_pending, putters') -> (Nothing, putters', mk_pending x : pendings)
      Just x  -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_putters = queue (x, k ()) (rtsvar_putters state) } ) >> scheduleM pendings next_tid scheduler


example1 = do
    yield
    v <- newEmptyRTSVar
    --putRTSVar v 3
    putRTSVar v 3
    yield
    --takeRTSVar v
    takeRTSVar v

example2 = do
    v_in <- newRTSVar 1336
    v_out <- newEmptyRTSVar
    forkIO $ do
        x <- takeRTSVar v_in
        yield
        putRTSVar v_out (x + 1)
    takeRTSVar v_out

-- An example with a race: depending on scheduling, this either returns "Hello" or "World"
example3 = do
    v <- newEmptyRTSVar
    forkIO $ putRTSVar v "Hello"
    forkIO $ putRTSVar v "World"
    takeRTSVar v


testScheduleSafe :: Eq r => (forall s. RTSM s r r) -> IO ()
-- Cuter:
--testScheduleSafe act = test $ \sched -> expected == runRTSM sched act
-- More flexible:
testScheduleSafe act = test $ \ss -> trace (show ss) $ expected == runRTSM (schedulerStreemed ss) act
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

