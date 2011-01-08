import Control.Monad.ST
import Data.STRef




data Node s a = Node { location :: Integer, value :: a, backwards_ref :: STRef s (Maybe (Node s a)) }

data MutableQueue s a = MQ { next_location_ref :: STRef s Integer, front_ref :: STRef s (Maybe (Node s a)), back_ref :: STRef s (Maybe (Node s a)) }
                      deriving (Eq)

new :: ST s (MutableQueue s a)
new = do
    next_location_ref <- newSTRef 0
    front_ref <- newSTRef Nothing
    back_ref <- newSTRef Nothing
    return $ MQ next_location_ref front_ref back_ref

enqueue :: a -> MutableQueue s a -> ST s (Location s a)
enqueue x q = do
    next_location_i <- readSTRef (next_location_ref q)
    writeSTRef (next_location_ref q) (next_location_i + 1)
    
    backwards_ref' <- newSTRef Nothing
    let node = Node next_location_i x backwards_ref'
    
    mb_front <- readSTRef (front_ref q)
    case mb_front of
      Nothing    -> do
          writeSTRef (back_ref q)  (Just node)
          writeSTRef (front_ref q) (Just node)
      Just front -> do
          mb_back <- readSTRef (back_ref q)
          case mb_back of
            Nothing   -> error "???"
            Just back -> do
              writeSTRef (back_ref q)         (Just node)
              writeSTRef (backwards_ref back) (Just node)
    
    return $ L next_location_i q

dequeue :: MutableQueue s a -> ST s (Maybe a)
dequeue q = do
    mb_front <- readSTRef (front_ref q)
    case mb_front of
      Nothing    -> return Nothing
      Just front -> do
          backwards <- readSTRef (backwards_ref front)
          writeSTRef (front_ref q) backwards
          return (Just (value front))


data Location s a = L Integer (MutableQueue s a)
                  deriving (Eq)

delete :: Location s a -> ST s (Maybe a)
delete (L location_i q) = go (front_ref q)
  where
    go this_ref = do
      mb_this <- readSTRef this_ref
      case mb_this of
        Nothing -> return Nothing
        Just this
          | location_i /= location this -> go (backwards_ref this)
          | otherwise                   -> do
            mb_backwards <- readSTRef (backwards_ref this)
            writeSTRef this_ref mb_backwards
            return (Just (value this))


main = print $ runST $ do
    q <- new
    enqueue 1 q
    enqueue 2 q
    Just x1 <- dequeue q
    enqueue 3 q
    loc4 <- enqueue 4 q
    Just x2 <- dequeue q
    enqueue 5 q
    Just 4 <- delete loc4
    Just x3 <- dequeue q
    Just x4 <- dequeue q
    Nothing <- dequeue q
    return (x1, x2, x3, x4) -- (1, 2, 3, 5)
