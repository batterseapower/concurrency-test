{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving #-}

import Control.Arrow ((***), first, second)
import Control.Exception

import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.List

import Test.LazySmallCheck

import Data.TagBits

import Debug.Trace

import System.IO.Unsafe

import Prelude hiding (catch)


{-# NOINLINE exceptionTrace #-}
exceptionTrace :: a -> a
exceptionTrace x = unsafePerformIO (evaluate x `catch` (\e -> trace ("Exception in pure code: " ++ show (e :: SomeException)) $ throw e))


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
    show xs | not (unsafeIsEvaluated xs) = "..."
    show (x :< xs) = show x ++ " :< " ++ show xs

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
    series = cons SS >< streamSeries
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


newtype Scheduler = Scheduler { schedule :: forall s r. [Pending s r] -> (Scheduler, Pending s r, [Pending s r]) }

unfair :: Scheduler
unfair = Scheduler schedule
  where
    schedule []     = error "Blocked indefinitely!"
    schedule (x:xs) = (unfair, x, xs)

roundRobin :: Scheduler
roundRobin = Scheduler schedule
  where
    schedule [] = error "Blocked indefinitely!"
    schedule xs = (roundRobin, last xs, init xs)

streamed :: Stream Nat -> Scheduler
streamed (i :< is) = Scheduler schedule
  where
    schedule [] = error "Blocked indefinitely!"
    schedule xs = (streamed is, x, xs')
      where 
        n = i `mod` genericLength xs -- A bit unsatisfactory because I really want a uniform chance of scheduling the available threads
        (x, xs') = genericDeleteAt xs n

schedulerStreemed :: SchedulerStreem -> Scheduler
schedulerStreemed (SS sss) = Scheduler (schedule sss)
  where
    schedule sss [] = error "Blocked indefinitely!"
    schedule sss xs = (schedulerStreemed (SS sss'), x, xs')
      where
        Streem n sss' = genericIndexStream sss (genericLength xs - 1 :: Nat)
        (x, xs') = genericDeleteAt xs n

instance Show Scheduler where
    show _ = "Scheduler" -- FIXME: have to be able to show failing schedulings

instance Serial Scheduler where
    series = cons schedulerStreemed >< series


-- | A pending coroutine
--
-- You almost always want to use (scheduleM (k : pendings)) rather than (unPending k pendings) because
-- scheduleM gives QuickCheck a chance to try other schedulings, whereas using unPending forces control
-- flow to continue in the current thread.
newtype Pending s r = Pending { unPending :: [Pending s r] -> Scheduler -> ST s r }
newtype RTSM s r a = RTSM { unRTSM :: (a -> Pending s r) -> Pending s r }

runRTSM :: Scheduler -> (forall s. RTSM s r r) -> r
runRTSM scheduler mx = runST (unPending (unRTSM mx (\x -> Pending $ \_pendings _scheduler -> return x)) [] scheduler)


instance Functor (RTSM s r) where
    fmap = liftM

instance Monad (RTSM s r) where
    return x = RTSM $ \k -> k x
    mx >>= fxmy = RTSM $ \k_y -> unRTSM mx (\x -> unRTSM (fxmy x) k_y)

-- | Give up control to the scheduler: yields should be used at every point where it is useful to allow QuickCheck
-- to try several different scheduling options.
--
-- It is certainly enough to yield on every bind operation. But this is too much (and it breaks the monad laws).
-- Quviq/PULSE yields just before every side-effecting operation. I think we can just yield after every side-effecting
-- operation and get the same results.
yield :: RTSM s r ()
yield = RTSM $ \k -> Pending $ \pendings -> scheduleM (k () : pendings)

scheduleM :: [Pending s r] -> Scheduler -> ST s r
scheduleM pendings scheduler = case schedule scheduler pendings of (scheduler, pending, pendings) -> unPending pending pendings scheduler


fork :: RTSM s r () -> RTSM s r ()
fork forkable = RTSM $ \k -> Pending $ \pendings -> scheduleM (k () : unRTSM forkable (\() -> Pending scheduleM) : pendings)


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
newRTSVarInternal mb_x = RTSM $ \k -> Pending $ \pendings scheduler -> newSTRef (RTSVarState mb_x emptyQueue emptyQueue) >>= \stref -> unPending (k (RTSVar stref)) pendings scheduler -- NB: unPending legitimate here because newRTSVarInternal cannot have externally visible side effects

takeRTSVar :: RTSVar s r a -> RTSM s r a
takeRTSVar rtsvar = RTSM $ \k -> Pending $ \pendings scheduler -> do
    state <- readSTRef (unRTSVar rtsvar)
    case rtsvar_data state of
       -- NB: we must guarantee that the woken thread doing a putRTSVar (if any) completes its operation since takeMVar has this guarantee
      Just x  -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_data = dat', rtsvar_putters = putters' }) >> scheduleM (k x : pendings') scheduler
        where (dat', putters', pendings') = case dequeue (rtsvar_putters state) of
                  Nothing                      -> (Nothing, emptyQueue, pendings)
                  Just ((x, putter), putters') -> (Just x, putters', putter : pendings)
      Nothing -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_takers = queue k (rtsvar_takers state) }) >> scheduleM pendings scheduler

putRTSVar :: RTSVar s r a -> a -> RTSM s r ()
putRTSVar rtsvar x = RTSM $ \k -> Pending $ \pendings scheduler -> do
    state <- readSTRef (unRTSVar rtsvar)
    case rtsvar_data state of
       -- NB: we must guarantee that the woken thread doing a takeRTSVar (if any) completes its operation since putMVar has this guarantee
      Nothing -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_data = dat', rtsvar_takers = takers' }) >> scheduleM (k () : pendings') scheduler
        where (dat', takers', pendings') = case dequeue (rtsvar_takers state) of
                  Nothing                     -> (Just x, emptyQueue, pendings)
                  Just (mk_pending, putters') -> (Nothing, putters', mk_pending x : pendings)
      Just x  -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_putters = queue (x, k ()) (rtsvar_putters state) } ) >> scheduleM pendings scheduler


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
    fork $ do
        x <- takeRTSVar v_in
        yield
        putRTSVar v_out (x + 1)
    takeRTSVar v_out

-- An example with a race: depending on scheduling, this either returns "Hello" or "World"
example3 = do
    v <- newEmptyRTSVar
    fork $ putRTSVar v "Hello"
    fork $ putRTSVar v "World"
    takeRTSVar v


testScheduleSafe :: Eq r => (forall s. RTSM s r r) -> IO ()
-- Cuter:
--testScheduleSafe act = test $ \sched -> expected == runRTSM sched act
-- More flexible:
testScheduleSafe act = test $ \ss -> (\res -> trace (res `seq` show ss) res) $ expected == runRTSM (schedulerStreemed ss) act
  where expected = runRTSM unfair act


main :: IO ()
main = do
    -- Demonstrates the presence of a data race - these two invocations return different results
    print $ runRTSM unfair example3
    print $ runRTSM roundRobin example3

    -- Let's see if we can find the race automatically!
    testScheduleSafe example3

