{-# LANGUAGE FlexibleContexts, TypeFamilies, RankNTypes #-}
module Control.Monad.Concurrent where

import Control.Exception (Exception(..), SomeException, MaskingState)
import qualified Control.Exception as IO
import qualified Control.Concurrent as IO

import Data.Typeable (Typeable, Typeable1)

import Prelude hiding (catch)


class Monad m => MonadException m where
    mask :: ((forall a. m a -> m a) -> m b) -> m b
    mask_ :: m a -> m a
    mask_ io = mask $ \_ -> io

    uninterruptibleMask :: ((forall a. m a -> m a) -> m b) -> m b
    uninterruptibleMask_ :: m a -> m a
    uninterruptibleMask_ io = uninterruptibleMask $ \_ -> io

    getMaskingState :: m MaskingState

    -- TODO: add other functions from Control.Exception
    
    throwIO :: Exception e => e -> m a
    throwTo :: Exception e => ThreadId m -> e -> m ()
    catch :: Exception e => m a -> (e -> m a) -> m a
    
    bracket :: m a -> (a -> m b) -> (a -> m c) -> m c
    bracket before after thing = mask $ \restore -> do
        a <- before
        r <- restore (thing a) `onException` after a
        _ <- after a
        return r

    bracket_ :: m a -> m b -> m c -> m c
    bracket_ before after thing = bracket before (const after) (const thing)

    onException :: m a -> m b -> m a
    onException io what = io `catch` handle
      where handle e = do
              _ <- what
              throwIO (e :: SomeException)

instance MonadException IO where
    mask = IO.mask
    mask_ = IO.mask_
    uninterruptibleMask = IO.uninterruptibleMask
    uninterruptibleMask_ = IO.uninterruptibleMask_
    getMaskingState = IO.getMaskingState
    
    throwIO = IO.throwIO
    throwTo = IO.throwTo
    catch = IO.catch
    
    bracket = IO.bracket
    bracket_ = IO.bracket_
    
    onException = IO.onException


class (Eq (ThreadId m),
       Ord (ThreadId m),
       Show (ThreadId m),
       Typeable (ThreadId m),
       Monad m) => MonadConcurrent m where
    type ThreadId m :: *
    
    forkIO :: m () -> m (ThreadId m)
    myThreadId :: m (ThreadId m)

    yield :: m ()

instance MonadConcurrent IO where
    type ThreadId IO = IO.ThreadId
    
    forkIO = IO.forkIO
    myThreadId = IO.myThreadId
    
    yield = IO.yield


-- TODO: we need to have (forall a. Eq (MVar m) a) in the context here but we can't
class (Typeable1 (MVar m),
       MonadException m, MonadConcurrent m) => MonadMVar m where
    type MVar m :: * -> *
    
    newEmptyMVar :: m (MVar m a)
    newMVar :: a -> m (MVar m a)
    
    takeMVar :: MVar m a -> m a
    putMVar :: MVar m a -> a -> m ()
    readMVar :: MVar m a -> m a
    readMVar m = mask_ $ do
        a <- takeMVar m
        putMVar m a
        return a
    
    swapMVar :: MVar m a -> a -> m a
    swapMVar m new = mask_ $ do
        old <- takeMVar m
        putMVar m new
        return old
    
    tryTakeMVar :: MVar m a -> m (Maybe a)
    tryPutMVar :: MVar m a -> a -> m Bool
    isEmptyMVar :: MVar m a -> m Bool
    
    withMVar :: MVar m a -> (a -> m b) -> m b
    withMVar m io = mask $ \restore -> do
        a <- takeMVar m
        b <- restore (io a) `onException` putMVar m a
        putMVar m a
        return b
    
    modifyMVar_ :: MVar m a -> (a -> m a) -> m ()
    modifyMVar_ m io = mask $ \restore -> do
        a  <- takeMVar m
        a' <- restore (io a) `onException` putMVar m a
        putMVar m a'
    
    modifyMVar :: MVar m a -> (a -> m (a, b)) -> m b
    modifyMVar m io = mask $ \restore -> do
        a      <- takeMVar m
        (a',b) <- restore (io a) `onException` putMVar m a
        putMVar m a'
        return b

instance MonadMVar IO where
    type MVar IO = IO.MVar
    
    newEmptyMVar = IO.newEmptyMVar
    newMVar = IO.newMVar
    
    takeMVar = IO.takeMVar
    putMVar = IO.putMVar
    readMVar = IO.readMVar
    
    swapMVar = IO.swapMVar
    
    tryTakeMVar = IO.tryTakeMVar
    tryPutMVar = IO.tryPutMVar
    isEmptyMVar = IO.isEmptyMVar
    
    withMVar = IO.withMVar
    modifyMVar_ = IO.modifyMVar_
    modifyMVar = IO.modifyMVar
