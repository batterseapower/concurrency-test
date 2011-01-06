{-# LANGUAGE Rank2Types #-}

import Control.Monad

import Control.Monad.ST
import Data.STRef


newtype Pending s b = Pending { unPending :: [Pending s b] -> ST s b }
newtype RTSM s a = RTSM { unRTSM :: forall b. (a -> Pending s b) -> Pending s b }

runRTSM :: (forall s. RTSM s a) -> a
runRTSM mx = runST (unPending (unRTSM mx (\x -> Pending $ \_pendings -> return x)) [])


instance Functor (RTSM s) where
    fmap = liftM

instance Monad (RTSM s) where
    return x = RTSM $ \k -> k x
    mx >>= fxmy = RTSM $ \k_y -> Pending $ \pendings -> unPending (unRTSM mx (\x -> unRTSM (fxmy x) k_y)) pendings

yield :: RTSM s ()
yield = RTSM $ \k -> Pending $ \pendings -> case schedule (k (), pendings) of (k, pendings) -> unPending k pendings

schedule :: (Pending s b, [Pending s b]) -> (Pending s b, [Pending s b])
schedule (x, xs) = (last (x:xs), init (x:xs)) -- Round robin scheduling (poor mans queue)


newtype RTSVar s a = RTSVar { unRTSVar :: STRef s (Maybe a) }

newEmptyRTSVar :: RTSM s (RTSVar s a)
newEmptyRTSVar = undefined


main :: IO ()
main = return ()
