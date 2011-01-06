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
schedule xs = (last xs, init xs) -- Round robin scheduling (poor mans queue)


data RTSVarState s r a = RTSVarState {
    rtsvar_data :: Maybe a,
    rtsvar_putters :: [Pending s r],
    rtsvar_takers :: [Pending s r]
  }

newtype RTSVar s r a = RTSVar { unRTSVar :: STRef s (RTSVarState s r a) }

newEmptyRTSVar :: RTSM s r (RTSVar s r a)
newEmptyRTSVar = newRTSVarInternal Nothing

newRTSVar :: a -> RTSM s r (RTSVar s r a)
newRTSVar = newRTSVarInternal . Just

newRTSVarInternal :: Maybe a -> RTSM s r (RTSVar s r a)
newRTSVarInternal mb_x = RTSM $ \k -> Pending $ \pendings -> newSTRef (RTSVarState mb_x [] []) >>= \stref -> unPending (k (RTSVar stref)) pendings

takeRTSVar :: RTSVar s r a -> RTSM s r a
takeRTSVar rtsvar = RTSM try_again
  where
    try_again k = Pending $ \pendings -> do
        state <- readSTRef (unRTSVar rtsvar)
        case rtsvar_data state of
          Just x  -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_data = Nothing, rtsvar_putters = putters' }) >> unPending (k x) (maybe id (:) mb_wake_putter pendings)
            where (mb_wake_putter, putters') = unCons (rtsvar_putters state) -- TODO: non-FCFS strategies?
          Nothing -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_takers = try_again k : rtsvar_takers state }) >> scheduleM pendings

putRTSVar :: RTSVar s r a -> a -> RTSM s r ()
putRTSVar rtsvar x = RTSM try_again
  where
    try_again k = Pending $ \pendings -> do
        state <- readSTRef (unRTSVar rtsvar)
        case rtsvar_data state of
          Nothing -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_data = Just x, rtsvar_takers = takers' }) >> unPending (k ()) (maybe id (:) mb_wake_taker pendings)
            where (mb_wake_taker, takers') = unCons (rtsvar_takers state) -- TODO: non-FCFS strategies?
          Just x  -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_putters = try_again k : rtsvar_putters state } ) >> scheduleM pendings

unCons :: [a] -> (Maybe a, [a])
unCons []     = (Nothing, [])
unCons (x:xs) = (Just x,  xs)


main :: IO ()
main = print $ runRTSM $ do
    yield
    v <- newEmptyRTSVar
    --putRTSVar v 3
    putRTSVar v 3
    yield
    --takeRTSVar v
    takeRTSVar v
