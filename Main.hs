{-# LANGUAGE Rank2Types #-}

import Control.Monad


newtype Pending b = Pending { unPending :: [Pending b] -> b }
newtype RTSM a = RTSM { unRTSM :: forall b. (a -> Pending b) -> Pending b }

runRTSM :: RTSM a -> a
runRTSM mx = unPending (unRTSM mx (\x -> Pending $ \_pendings -> x)) []


instance Functor RTSM where
    fmap = liftM

instance Monad RTSM where
    return x = RTSM $ \k -> k x
    mx >>= fxmy = RTSM $ \k_y -> Pending $ \pendings -> unPending (unRTSM mx (\x -> unRTSM (fxmy x) k_y)) pendings

yield :: RTSM ()
yield = RTSM $ \k -> Pending $ \pendings -> case schedule (k (), pendings) of (k, pendings) -> unPending k pendings

schedule :: (Pending b, [Pending b]) -> (Pending b, [Pending b])
schedule (x, xs) = (last (x:xs), init (x:xs)) -- Round robin scheduling (poor mans queue)


main :: IO ()
main = return ()
