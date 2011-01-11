{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
import Control.Monad.ST
import Debug.Trace


data Result a = Good a | Bad
              deriving (Show)


newtype ThreadId = TID Int
                 deriving (Show)


newtype Unblocked s r = Unblocked { unUnblocked :: ([Unblocked s r] -> Int -> ST s (Result r))
                                                -> Int -- Next TID
                                                -> ST s (Result r) }

newtype RTS s r a = RTS { unRTS :: (a
                                    -> ([Unblocked s r] -> Int -> ST s (Result r))
                                    -> Int -- Next TID
                                    -> ST s (Result r))
                                -> ([Unblocked s r]
                                    -> Int -- Next TID
                                    -> ST s (Result r))
                                -> ThreadId -- My TID
                                -> Int -- Next TID
                                -> ST s (Result r) }

instance Monad (RTS s r) where
    return x = RTS $ \k_done k_may_block _tid next_tid -> k_done x k_may_block next_tid
    mx >>= fxmy = RTS $ \k_done k_may_block tid next_tid -> unRTS mx (\x k_may_block next_tid -> unRTS (fxmy x) k_done k_may_block tid next_tid) k_may_block tid next_tid

yield :: RTS s r ()
yield = RTS $ \k_done k_may_block _tid next_tid -> trace ("Thread " ++ show _tid ++ " yielding") $ k_may_block [Unblocked $ \k_may_block next_tid -> k_done () k_may_block next_tid] next_tid

forkIO :: RTS s r () -> RTS s r Int -- NB: uses schedule directly because we wish to run both new threads until they reach a sync object operation
forkIO rts = RTS $ \k_done k_may_block _tid next_tid -> k_may_block [Unblocked $ \k_may_block next_tid' -> k_done next_tid {- Return to caller -} k_may_block next_tid',
                                                                     Unblocked $ \k_may_block next_tid' -> unRTS rts (\() k_may_block next_tid -> k_may_block [] next_tid) k_may_block (TID next_tid) {- Assign new ThreadIs -} next_tid']
                                                                    (next_tid + 1)

runRTS :: (forall s. RTS s r r) -> Result r
runRTS rts = runST $ unRTS rts (\x _k_may_block _next_tid -> return (Good x)) schedule (TID 0) 1
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
