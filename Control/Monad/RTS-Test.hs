{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
import Control.Monad.ST
import Debug.Trace


data Result a = Good a | Bad
              deriving (Show)


newtype ThreadId = TID Int
                 deriving (Show)


newtype Unblocked' s r a = Unblocked { unUnblocked :: ([Unblocked s r] -> Int -> ST s (Result r))
                                                   -> Int -- Next TID
                                                   -> ST s a }

instance Monad (Unblocked' s r) where
    return x = Unblocked $ \_k_may_block _tid -> return x
    mx >>= fxmy = Unblocked $ \k_may_block tid -> unUnblocked mx k_may_block tid >>= \x -> unUnblocked (fxmy x) k_may_block tid

reschedule :: [Unblocked s r] -> Unblocked s r
reschedule unblockeds = Unblocked $ \k_may_block next_tid -> k_may_block unblockeds next_tid

type Unblocked s r = Unblocked' s r (Result r)
newtype RTS s r a = RTS { unRTS :: (a -> Unblocked s r)
                                -> ThreadId -- My TID
                                -> Unblocked s r }

instance Monad (RTS s r) where
    return x = RTS $ \k_done _tid -> k_done x
    mx >>= fxmy = RTS $ \k_done tid -> unRTS mx (\x -> unRTS (fxmy x) k_done tid) tid

yield :: RTS s r ()
yield = RTS $ \k_done _tid -> trace ("Thread " ++ show _tid ++ " yielding") $ reschedule [k_done ()]

forkIO :: RTS s r () -> RTS s r Int
forkIO rts = RTS $ \k_done _tid -> Unblocked $ \k_may_block next_tid -> k_may_block [k_done next_tid,
                                                                                     unRTS rts (\() -> reschedule []) (TID next_tid)]
                                                                                    (next_tid + 1)

runRTS :: (forall s. RTS s r r) -> Result r
runRTS rts = runST $ unUnblocked (unRTS rts (\x -> return (Good x)) (TID 0)) schedule 1
  where
    -- Dummy impl:
    schedule :: [Unblocked s r] -> Int -> ST s (Result r)
    schedule []                     _next_tid = error "Blocked forever"
    schedule (unblocked:unblockeds) next_tid  = {- trace ("Scheduling " ++ show (length unblockeds + 1) ++ " threads") $ -} unUnblocked unblocked (\unblockeds' -> schedule (unblockeds ++ unblockeds')) next_tid


main :: IO ()
main = print $ runRTS $ do
    1 <- forkIO $ do
      return True
      2 <- forkIO $ do
        return "Hello"
        yield
        yield
        return ()
      return False
      yield
      yield
      yield
    
    yield
    yield
    yield
    yield
    
    return "10"
    yield
    return "2"
