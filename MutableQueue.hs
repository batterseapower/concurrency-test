import Control.Monad.ST
import Data.STRef


-- Initiall based on a Haskell translation of <http://www.java-tips.org/java-se-tips/java.lang/linked-list-based-queue-implementation.html>


data Node s a = Node { location :: Integer, value :: a, backwards_ref :: STRef s (Maybe (Node s a)) }

data Front s a = Empty
               | NonEmpty (Node s a) (Node s a)
data MutableQueue s a = MQ { next_location_ref :: STRef s Integer, front_ref :: STRef s (Front s a) }
                      deriving (Eq)

new :: ST s (MutableQueue s a)
new = do
    next_location_ref <- newSTRef 0
    front_ref <- newSTRef Empty
    return $ MQ next_location_ref front_ref

enqueue :: a -> MutableQueue s a -> ST s (Location s a)
enqueue x q = do
    next_location_i <- readSTRef (next_location_ref q)
    writeSTRef (next_location_ref q) (next_location_i + 1)
    
    backwards_ref' <- newSTRef Nothing
    let node = Node next_location_i x backwards_ref'
    
    front <- readSTRef (front_ref q)
    case front of
      Empty -> do
          writeSTRef (front_ref q) $ NonEmpty node node
      NonEmpty front back -> do
          writeSTRef (front_ref q) $ NonEmpty front node
          writeSTRef (backwards_ref back) (Just node)
    
    return $ L next_location_i q

dequeue :: MutableQueue s a -> ST s (Maybe a)
dequeue q = do
    mb_front <- readSTRef (front_ref q)
    case mb_front of
      Empty -> return Nothing
      NonEmpty front back -> do
          mb_backwards <- readSTRef (backwards_ref front)
          writeSTRef (front_ref q) (maybe Empty (\backwards -> NonEmpty backwards back) mb_backwards)
          return (Just (value front))


data Location s a = L Integer (MutableQueue s a)
                  deriving (Eq)

delete :: Location s a -> ST s (Maybe a)
delete (L location_i q) = do
    front <- readSTRef (front_ref q)
    case front of
      Empty               -> return Nothing
      NonEmpty front back -> do
         go (Just front) (\mb_front -> writeSTRef (front_ref q) (maybe Empty (\front -> NonEmpty front back) mb_front))
  where
    go Nothing     _ = return Nothing
    go (Just this) write_this_ref
      | location_i /= location this = do
        mb_backwards <- readSTRef (backwards_ref this)
        go mb_backwards (writeSTRef (backwards_ref this))
      | otherwise                   = do
        mb_backwards <- readSTRef (backwards_ref this)
        write_this_ref mb_backwards
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
