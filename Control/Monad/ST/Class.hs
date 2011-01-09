{-# LANGUAGE TypeFamilies #-}
module Control.Monad.ST.Class (MonadST(..)) where

import Control.Monad.ST


-- | Type class of monads that can perform lifted computation in the 'ST' monad.
class Monad m => MonadST m where
    type StateThread m
    liftST :: ST (StateThread m) a -> m a

instance MonadST (ST s) where
    type StateThread (ST s) = s
    liftST m = m

instance MonadST IO where
    type StateThread IO = RealWorld
    liftST = stToIO
