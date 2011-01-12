module Data.STQueue (
    STQueue, Location,
    new, Data.STQueue.null, enqueue, dequeue, delete,
    toList, toListWithLocation, mapMaybeM
  ) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Class
import Data.STRef

import Utilities (pamf)


-- Initiall based on a Haskell translation of <http://www.java-tips.org/java-se-tips/java.lang/linked-list-based-queue-implementation.html>


data Node s a = Node { location :: Integer, value :: a, backwards_ref :: STRef s (Maybe (Node s a)) }

data Front s a = Empty
               | NonEmpty (Node s a) (Node s a)
data STQueue s a = MQ { next_location_ref :: STRef s Integer, front_ref :: STRef s (Front s a) }
                      deriving (Eq)

new :: ST s (STQueue s a)
new = do
    next_location_ref <- newSTRef 0
    front_ref <- newSTRef Empty
    return $ MQ next_location_ref front_ref

null :: STQueue s a -> ST s Bool
null q = readSTRef (front_ref q) `pamf` \front -> case front of
      Empty        -> True
      NonEmpty _ _ -> False

enqueue :: a -> STQueue s a -> ST s (Location s a)
enqueue x q = do
    next_location_i <- readSTRef (next_location_ref q)
    writeSTRef (next_location_ref q) (next_location_i + 1)
    
    enqueueInLocation next_location_i x q
    return $ L next_location_i q

enqueueInLocation :: Integer -> a -> STQueue s a -> ST s ()
enqueueInLocation next_location_i x q = do
    backwards_ref' <- newSTRef Nothing
    let node = Node next_location_i x backwards_ref'
    
    front <- readSTRef (front_ref q)
    case front of
      Empty -> do
          writeSTRef (front_ref q) $ NonEmpty node node
      NonEmpty front back -> do
          writeSTRef (front_ref q) $ NonEmpty front node
          writeSTRef (backwards_ref back) (Just node)

dequeue :: STQueue s a -> ST s (Maybe a)
dequeue = fmap (fmap snd) . dequeueWithLocation

dequeueWithLocation :: STQueue s a -> ST s (Maybe (Integer, a))
dequeueWithLocation q = do
    mb_front <- readSTRef (front_ref q)
    case mb_front of
      Empty -> return Nothing
      NonEmpty front back -> do
          mb_backwards <- readSTRef (backwards_ref front)
          replaceFront q mb_backwards back
          return (Just (location front, value front))

replaceFront :: STQueue s a -> Maybe (Node s a) -> Node s a -> ST s ()
replaceFront q mb_front back = writeSTRef (front_ref q) (maybe Empty (\front -> NonEmpty front back) mb_front)


toList :: STQueue s a -> ST s [a]
toList = fmap (map snd) . toListWithLocation

toListWithLocation :: STQueue s a -> ST s [(Integer, a)]
toListWithLocation q = do
   mb_front <- readSTRef (front_ref q)
   case mb_front of
     Empty -> return []
     NonEmpty front _back -> go [] front
       where
         go acc node = do
            mb_backwards <- readSTRef (backwards_ref node)
            let acc' = (location node, value node) : acc
            maybe (return (reverse acc')) (\backwards -> go acc' backwards) mb_backwards


mapMaybeM :: MonadST m => (a -> m (Maybe a)) -> STQueue (StateThread m) a -> m ()
mapMaybeM f q = go []
  where
    go ys = do
      mb_x <- liftST $ dequeueWithLocation q
      case mb_x of
        Nothing -> forM_ (reverse ys) $ \(loc, y) -> liftST $ enqueueInLocation loc y q
        Just (loc, x) -> do
          mb_y <- f x
          go (maybe ys (\y -> (loc, y) : ys) mb_y)


data Location s a = L Integer (STQueue s a) -- TODO: could probably implement constant-time delete if I could tolerate more pointers
                  deriving (Eq)

delete :: Location s a -> ST s (Maybe a)
delete (L location_i q) = do
    front <- readSTRef (front_ref q)
    case front of
      Empty               -> return Nothing
      NonEmpty front back -> go front (\mb_front -> replaceFront q mb_front back)
  where
    go this write_this_ref
      | location_i /= location this = do
        mb_backwards <- readSTRef (backwards_ref this)
        maybe (return Nothing) (\backwards -> go backwards (writeSTRef (backwards_ref this))) mb_backwards
      | otherwise                   = do
        mb_backwards <- readSTRef (backwards_ref this)
        write_this_ref mb_backwards
        return (Just (value this))


-- main :: IO ()
-- main = print (runST test :: (Int, Int, Int, Int))
--   where
--     test = do
--       q <- new
--       enqueue 1 q
--       enqueue 2 q
--       Just x1 <- dequeue q
--       enqueue 3 q
--       loc4 <- enqueue 4 q
--       Just x2 <- dequeue q
--       enqueue 5 q
--       Just 4 <- delete loc4
--       Just x3 <- dequeue q
--       Just x4 <- dequeue q
--       Nothing <- dequeue q
--       return (x1, x2, x3, x4) -- (1, 2, 3, 5)
