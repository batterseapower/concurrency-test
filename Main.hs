{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving #-}

import Control.Arrow ((***), first, second)
import Control.Applicative (Applicative(..))
import Control.Monad
import Control.Exception

import Control.Monad.ST
import Data.STRef
import Data.List
import Data.Monoid (mempty)
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))

import Test.LazySmallCheck
import Test.QuickCheck hiding (Success, Result, (><))
import Test.QuickCheck.Gen

import System.Random

import Prelude hiding (catch)


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
exceptionTrace x = unsafePerformIO (evaluate x `catch` (\e -> trace ("Exception in pure code: " ++ show (e :: SomeException)) $ throw e))

-- I used to use unsafeIsEvaluated to decide where to put in "...", but that pruned too heavily because
-- I couldn't show the schedule before it was actually poked on and those thunks turned into real values.
{-# NOINLINE showsExplored #-}
showsExplored :: (a -> ShowS) -> a -> ShowS
showsExplored shows x = unsafePerformIO $ fmap (maybe (showString "...") shows) $ tryIf isLSCError (evaluate x)
  where
    -- Looked at the LSC code to see what sort of errors it was generating...
    isLSCError (ErrorCall ('\0':msg)) = True
    isLSCError _                      = False
    
    tryIf :: Exception e => (e -> Bool) -> IO a -> IO (Maybe a)
    tryIf p act = fmap (either (\() -> Nothing) Just) $ tryJust (\e -> guard (p e) >> return ()) act


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


class Scheduler h where
    schedule :: h -> [Pending h s r] -> Result (h, Pending h s r, [Pending h s r])

data Unfair = Unfair
instance Scheduler Unfair where
    schedule Unfair []     = BlockedIndefinitely
    schedule Unfair (x:xs) = Success (Unfair, x, xs)

data RoundRobin = RoundRobin
instance Scheduler RoundRobin where
    schedule RoundRobin [] = BlockedIndefinitely
    schedule RoundRobin xs = Success (RoundRobin, last xs, init xs)

data Streamed = Streamed (Stream Nat)
instance Scheduler Streamed where
    schedule (Streamed _)         [] = BlockedIndefinitely
    schedule (Streamed (i :< is)) xs = Success (Streamed is, x, xs')
      where 
        n = i `mod` genericLength xs -- A bit unsatisfactory because I really want a uniform chance of scheduling the available threads
        (x, xs') = genericDeleteAt xs n

data SchedulerStreemed = SchedulerStreemed SchedulerStreem
instance Scheduler SchedulerStreemed where
    schedule (SchedulerStreemed _)        [] = BlockedIndefinitely
    schedule (SchedulerStreemed (SS sss)) xs = Success (SchedulerStreemed (SS sss'), x, xs')
      where
        Streem n sss' = genericIndexStream sss (genericLength xs - 1 :: Nat)
        (x, xs') = genericDeleteAt xs n

instance Show SchedulerStreemed where
    show _ = "SchedulerStreemed" -- FIXME: have to be able to show failing schedulings in a nice way

instance Serial SchedulerStreemed where
    series = cons SchedulerStreemed >< series

data Randomised = Randomised StdGen
instance Scheduler Randomised where
    schedule (Randomised gen) [] = BlockedIndefinitely
    schedule (Randomised gen) xs = Success (Randomised gen', x, xs')
      where
        (n, gen') = randomR (0, length xs - 1) gen
        (x, xs') = genericDeleteAt xs n


data Result a = Success a
              | BlockedIndefinitely
              deriving (Eq, Show)

instance Functor Result where
    fmap = liftM

instance Applicative Result where
    pure = return
    (<*>) = ap

instance Monad Result where
    return = Success
    Success x           >>= f = f x
    BlockedIndefinitely >>= f = BlockedIndefinitely

instance Foldable Result where
    foldMap f (Success x)         = f x
    foldMap _ BlockedIndefinitely = mempty

instance Traversable Result where
    traverse f (Success x)         = pure Success <*> f x
    traverse _ BlockedIndefinitely = pure BlockedIndefinitely


-- | A pending coroutine
--
-- You almost always want to use (scheduleM (k : pendings)) rather than (unPending k pendings) because
-- scheduleM gives QuickCheck a chance to try other schedulings, whereas using unPending forces control
-- flow to continue in the current thread.
newtype Pending h s r = Pending { unPending :: [Pending h s r] -> h -> ST s (Result r) }
newtype RTSM h s r a = RTSM { unRTSM :: (a -> Pending h s r) -> Pending h s r }

runRTSM :: h -> (forall s. RTSM h s r r) -> Result r
runRTSM scheduler mx = runST (unPending (unRTSM mx (\x -> Pending $ \_pendings _scheduler -> return (Success x))) [] scheduler)


instance Functor (RTSM h s r) where
    fmap = liftM

instance Applicative (RTSM h s r) where
   pure = return
   (<*>) = ap

instance Monad (RTSM h s r) where
    return x = RTSM $ \k -> k x
    mx >>= fxmy = RTSM $ \k_y -> unRTSM mx (\x -> unRTSM (fxmy x) k_y)

-- | Give up control to the scheduler: yields should be used at every point where it is useful to allow QuickCheck
-- to try several different scheduling options.
--
-- It is certainly enough to yield on every bind operation. But this is too much (and it breaks the monad laws).
-- Quviq/PULSE yields just before every side-effecting operation. I think we can just yield after every side-effecting
-- operation and get the same results.
yield :: Scheduler h => RTSM h s r ()
yield = RTSM $ \k -> Pending $ \pendings -> scheduleM (k () : pendings)

scheduleM :: Scheduler h => [Pending h s r] -> h -> ST s (Result r)
scheduleM pendings scheduler = fmap join $ traverse (\(scheduler, pending, pendings) -> unPending pending pendings scheduler) (schedule scheduler pendings)


fork :: Scheduler h => RTSM h s r () -> RTSM h s r ()
fork forkable = RTSM $ \k -> Pending $ \pendings -> scheduleM (k () : unRTSM forkable (\() -> Pending scheduleM) : pendings)


data RTSVarState h s r a = RTSVarState {
    rtsvar_data :: Maybe a,
    -- MVars have guaranteed FIFO semantics, so that's what we do here
    rtsvar_putters :: Queue (a, Pending h s r),
    rtsvar_takers :: Queue (a -> Pending h s r)
  }

newtype RTSVar h s r a = RTSVar { unRTSVar :: STRef s (RTSVarState h s r a) } -- TODO: I could detect more unreachable states if I find that a RTSVar currently blocking a Pending gets GCed

newEmptyRTSVar :: RTSM h s r (RTSVar h s r a)
newEmptyRTSVar = newRTSVarInternal Nothing

newRTSVar :: a -> RTSM h s r (RTSVar h s r a)
newRTSVar = newRTSVarInternal . Just

newRTSVarInternal :: Maybe a -> RTSM h s r (RTSVar h s r a)
newRTSVarInternal mb_x = RTSM $ \k -> Pending $ \pendings scheduler -> newSTRef (RTSVarState mb_x emptyQueue emptyQueue) >>= \stref -> unPending (k (RTSVar stref)) pendings scheduler -- NB: unPending legitimate here because newRTSVarInternal cannot have externally visible side effects

takeRTSVar :: Scheduler h => RTSVar h s r a -> RTSM h s r a
takeRTSVar rtsvar = RTSM $ \k -> Pending $ \pendings scheduler -> do
    state <- readSTRef (unRTSVar rtsvar)
    case rtsvar_data state of
       -- NB: we must guarantee that the woken thread doing a putRTSVar (if any) completes its operation since takeMVar has this guarantee
      Just x  -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_data = dat', rtsvar_putters = putters' }) >> scheduleM (k x : pendings') scheduler
        where (dat', putters', pendings') = case dequeue (rtsvar_putters state) of
                  Nothing                      -> (Nothing, emptyQueue, pendings)
                  Just ((x, putter), putters') -> (Just x, putters', putter : pendings)
      Nothing -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_takers = queue k (rtsvar_takers state) }) >> scheduleM pendings scheduler

putRTSVar :: Scheduler h => RTSVar h s r a -> a -> RTSM h s r ()
putRTSVar rtsvar x = RTSM $ \k -> Pending $ \pendings scheduler -> do
    state <- readSTRef (unRTSVar rtsvar)
    case rtsvar_data state of
       -- NB: we must guarantee that the woken thread doing a takeRTSVar (if any) completes its operation since putMVar has this guarantee
      Nothing -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_data = dat', rtsvar_takers = takers' }) >> scheduleM (k () : pendings') scheduler
        where (dat', takers', pendings') = case dequeue (rtsvar_takers state) of
                  Nothing                     -> (Just x, emptyQueue, pendings)
                  Just (mk_pending, putters') -> (Nothing, putters', mk_pending x : pendings)
      Just x  -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_putters = queue (x, k ()) (rtsvar_putters state) } ) >> scheduleM pendings scheduler


example1 :: Scheduler h => RTSM h s r Int
example1 = do
    yield
    v <- newEmptyRTSVar
    --putRTSVar v 3
    putRTSVar v 3
    yield
    --takeRTSVar v
    takeRTSVar v

example2 :: Scheduler h => RTSM h s r Int
example2 = do
    v_in <- newRTSVar 1336
    v_out <- newEmptyRTSVar
    fork $ do
        x <- takeRTSVar v_in
        yield
        putRTSVar v_out (x + 1)
    takeRTSVar v_out

-- An example with a race: depending on scheduling, this either returns "Hello" or "World"
example3 :: Scheduler h => RTSM h s r String
example3 = do
    v <- newEmptyRTSVar
    fork $ putRTSVar v "Hello"
    fork $ putRTSVar v "World"
    takeRTSVar v


testScheduleSafe :: Eq r => (forall h s. Scheduler h => RTSM h s r r) -> IO ()
-- Cuter:
--testScheduleSafe act = test $ \sched -> expected == runRTSM sched act
-- More flexible:
testScheduleSafe act = test $ \ss -> trace (show ss) $ expected == runRTSM (SchedulerStreemed ss) act
-- Working:
--testScheduleSafe act = quickCheck $ \gen -> expected == runRTSM (randomised gen) act
  where expected = runRTSM RoundRobin act


main :: IO ()
main = do
    -- Demonstrates the presence of a data race - these two invocations return different results
    print $ runRTSM Unfair example3
    print $ runRTSM RoundRobin example3

    -- Let's see if we can find the race automatically!
    testScheduleSafe example3

