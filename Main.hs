{-# LANGUAGE Rank2Types #-}

import Control.Monad

import Control.Monad.ST
import Data.STRef


newtype Pending s = Pending { unPending :: [Pending s] -> ST s () }
newtype RTSM s a = RTSM { unRTSM :: (a -> Pending s) -> Pending s }

runRTSM :: (forall s. RTSM s a) -> () -- TODO: figure out a way of returning data that still lets me write a type for the [Pending s] list in RTSVar
runRTSM mx = runST (unPending (unRTSM mx (\_x -> Pending $ \_pendings -> return ())) [])


instance Functor (RTSM s) where
    fmap = liftM

instance Monad (RTSM s) where
    return x = RTSM $ \k -> k x
    mx >>= fxmy = RTSM $ \k_y -> Pending $ \pendings -> unPending (unRTSM mx (\x -> unRTSM (fxmy x) k_y)) pendings

yield :: RTSM s ()
yield = RTSM $ \k -> Pending $ \pendings -> scheduleM (k () : pendings)

scheduleM :: [Pending s] -> ST s ()
scheduleM pendings = case schedule pendings of (pending, pendings) -> unPending pending pendings

schedule :: [Pending s] -> (Pending s, [Pending s])
schedule [] = error "Blocked indefinitely!"
schedule xs = (last xs, init xs) -- Round robin scheduling (poor mans queue)


data RTSVarState s a = RTSVarState {
    rtsvar_data :: Maybe a,
    rtsvar_putters :: [Pending s],
    rtsvar_takers :: [Pending s]
  }

newtype RTSVar s a = RTSVar { unRTSVar :: STRef s (RTSVarState s a) }

newEmptyRTSVar :: RTSM s (RTSVar s a)
newEmptyRTSVar = newRTSVarInternal Nothing

newRTSVar :: a -> RTSM s (RTSVar s a)
newRTSVar = newRTSVarInternal . Just

newRTSVarInternal :: Maybe a -> RTSM s (RTSVar s a)
newRTSVarInternal mb_x = RTSM $ \k -> Pending $ \pendings -> newSTRef (RTSVarState mb_x [] []) >>= \stref -> unPending (k (RTSVar stref)) pendings

takeRTSVar :: RTSVar s a -> RTSM s a
takeRTSVar rtsvar = RTSM try_again
  where
    try_again k = Pending $ \pendings -> do
        state <- readSTRef (unRTSVar rtsvar)
        case rtsvar_data state of
          Just x  -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_data = Nothing, rtsvar_putters = putters' }) >> unPending (k x) (maybe id (:) mb_wake_putter pendings)
            where (mb_wake_putter, putters') = unCons (rtsvar_putters state) -- TODO: non-FCFS strategies?
          Nothing -> writeSTRef (unRTSVar rtsvar) (state { rtsvar_takers = try_again k : rtsvar_takers state }) >> scheduleM pendings

putRTSVar :: RTSVar s a -> a -> RTSM s ()
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
main = return ()
