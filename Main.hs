{-# LANGUAGE Rank2Types #-}

import Control.Monad

import Control.Monad.ST
import Data.STRef


newtype Pending s r = Pending { unPending :: [Pending s r] -> ST s r }
newtype RTSM s r a = RTSM { unRTSM :: (a -> Pending s r) -> Pending s r }

runRTSM :: (forall s. RTSM s r r) -> r
runRTSM mx = runST (unPending (unRTSM mx (\x -> Pending $ \_pendings -> return x)) [])


instance Functor (RTSM s r) where
    fmap = liftM

instance Monad (RTSM s r) where
    return x = RTSM $ \k -> k x
    mx >>= fxmy = RTSM $ \k_y -> Pending $ \pendings -> unPending (unRTSM mx (\x -> unRTSM (fxmy x) k_y)) pendings

yield :: RTSM s r ()
yield = RTSM $ \k -> Pending $ \pendings -> scheduleM (k () : pendings)

scheduleM :: [Pending s r] -> ST s r
scheduleM pendings = case schedule pendings of (pending, pendings) -> unPending pending pendings

schedule :: [Pending s r] -> (Pending s r, [Pending s r])
schedule [] = error "Blocked indefinitely!"
schedule xs = (last xs, init xs) -- Round robin scheduling (poor mans queue). TODO: other scheduling strategies


fork :: RTSM s r () -> RTSM s r ()
fork forkable = RTSM $ \k -> Pending $ \pendings -> unPending (k ()) (unRTSM forkable (\() -> Pending scheduleM) : pendings)


data RTSVarState s r a = RTSVarState {
    rtsvar_data :: Maybe a,
    -- MVars have guaranteed FIFO semantics, so that's what we do here
    rtsvar_putters :: Queue (Pending s r),
    rtsvar_takers :: Queue (Pending s r)
  }

newtype RTSVar s r a = RTSVar { unRTSVar :: STRef s (RTSVarState s r a) }

newEmptyRTSVar :: RTSM s r (RTSVar s r a)
newEmptyRTSVar = newRTSVarInternal Nothing

newRTSVar :: a -> RTSM s r (RTSVar s r a)
newRTSVar = newRTSVarInternal . Just

newRTSVarInternal :: Maybe a -> RTSM s r (RTSVar s r a)
newRTSVarInternal mb_x = RTSM $ \k -> Pending $ \pendings -> newSTRef (RTSVarState mb_x emptyQueue emptyQueue) >>= \stref -> unPending (k (RTSVar stref)) pendings

takeRTSVar :: RTSVar s r a -> RTSM s r a
takeRTSVar rtsvar = RTSM try_again
  where
    try_again k = Pending $ \pendings -> do
        state <- readSTRef (unRTSVar rtsvar)
        case rtsvar_data state of
           -- TODO: we must guarantee that the woken thread completes its takeRTSVar operation since takeMVar does
          Just x  -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_data = Nothing, rtsvar_putters = putters' }) >> unPending (k x) (maybe id (:) mb_wake_putter pendings)
            where (mb_wake_putter, putters') = dequeue1 (rtsvar_putters state)
          Nothing -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_takers = queue (try_again k) (rtsvar_takers state) }) >> scheduleM pendings

putRTSVar :: RTSVar s r a -> a -> RTSM s r ()
putRTSVar rtsvar x = RTSM try_again
  where
    try_again k = Pending $ \pendings -> do
        state <- readSTRef (unRTSVar rtsvar)
        case rtsvar_data state of
           -- TODO: we must guarantee that the woken thread completes its putRTSVar operation since takeMVar does
          Nothing -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_data = Just x, rtsvar_takers = takers' }) >> unPending (k ()) (maybe id (:) mb_wake_taker pendings)
            where (mb_wake_taker, takers') = dequeue1 (rtsvar_takers state)
          Just x  -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_putters = queue (try_again k) (rtsvar_putters state) } ) >> scheduleM pendings


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

dequeue1 :: Queue a -> (Maybe a, Queue a)
dequeue1 = maybe (Nothing, emptyQueue) (\(x, xs) -> (Just x, xs)) . dequeue


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
