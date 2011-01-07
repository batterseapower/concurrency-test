{-# LANGUAGE Rank2Types #-}

import Control.Monad

import Control.Monad.ST
import Data.STRef

-- | A pending coroutine
--
-- You almost always want to use (scheduleM (k : pendings)) rather than (unPending k pendings) because
-- scheduleM gives QuickCheck a chance to try other schedulings, wherease using unPending forces control
-- flow to continue in the current thread.
newtype Pending s r = Pending { unPending :: [Pending s r] -> ST s r }
newtype RTSM s r a = RTSM { unRTSM :: (a -> Pending s r) -> Pending s r }

runRTSM :: (forall s. RTSM s r r) -> r
runRTSM mx = runST (unPending (unRTSM mx (\x -> Pending $ \_pendings -> return x)) [])


instance Functor (RTSM s r) where
    fmap = liftM

instance Monad (RTSM s r) where
    return x = RTSM $ \k -> k x
    mx >>= fxmy = RTSM $ \k_y -> Pending $ \pendings -> unPending (unRTSM mx (\x -> unRTSM (fxmy x) k_y)) pendings

-- | Give up control to the scheduler: yields should be used at every point where it is useful to allow QuickCheck
-- to try several different scheduling options.
--
-- It is certainly enough to yield on every bind operation. But this is too much (and it breaks the monad laws).
-- Quviq/PULSE yields just before every side-effecting operation. I think we can just yield after every side-effecting
-- operation and get the same results.
yield :: RTSM s r ()
yield = RTSM $ \k -> Pending $ \pendings -> scheduleM (k () : pendings)

scheduleM :: [Pending s r] -> ST s r
scheduleM pendings = case schedule pendings of (pending, pendings) -> unPending pending pendings

schedule :: [Pending s r] -> (Pending s r, [Pending s r])
schedule [] = error "Blocked indefinitely!"
schedule xs = (last xs, init xs) -- Round robin scheduling (poor mans queue). TODO: other scheduling strategies


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
newRTSVarInternal mb_x = RTSM $ \k -> Pending $ \pendings -> newSTRef (RTSVarState mb_x emptyQueue emptyQueue) >>= \stref -> unPending (k (RTSVar stref)) pendings -- NB: unPending legitimate here because newRTSVarInternal cannot have externally visible side effects

takeRTSVar :: RTSVar s r a -> RTSM s r a
takeRTSVar rtsvar = RTSM $ \k -> Pending $ \pendings -> do
    state <- readSTRef (unRTSVar rtsvar)
    case rtsvar_data state of
       -- NB: we must guarantee that the woken thread doing a putRTSVar (if any) completes its operation since takeMVar has this guarantee
      Just x  -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_data = dat', rtsvar_putters = putters' }) >> scheduleM (k x : pendings')
        where (dat', putters', pendings') = case dequeue (rtsvar_putters state) of
                  Nothing                      -> (Nothing, emptyQueue, pendings)
                  Just ((x, putter), putters') -> (Just x, putters', putter : pendings)
      Nothing -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_takers = queue k (rtsvar_takers state) }) >> scheduleM pendings

putRTSVar :: RTSVar s r a -> a -> RTSM s r ()
putRTSVar rtsvar x = RTSM $ \k -> Pending $ \pendings -> do
    state <- readSTRef (unRTSVar rtsvar)
    case rtsvar_data state of
       -- NB: we must guarantee that the woken thread doing a takeRTSVar (if any) completes its operation since putMVar has this guarantee
      Nothing -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_data = dat', rtsvar_takers = takers' }) >> scheduleM (k () : pendings')
        where (dat', takers', pendings') = case dequeue (rtsvar_takers state) of
                  Nothing                     -> (Just x, emptyQueue, pendings)
                  Just (mk_pending, putters') -> (Nothing, putters', mk_pending x : pendings)
      Just x  -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_putters = queue (x, k ()) (rtsvar_putters state) } ) >> scheduleM pendings


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

main :: IO ()
main = print $ runRTSM example2
