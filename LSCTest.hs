import Control.Arrow (first)

import Test.LazySmallCheck


prop_list :: [Int] -> Bool
prop_list xs = xs `lengthAtLeast` 4 ==> ((before ++ 9 : after) !! 4) == 9
  where (before, after) = splitAt 4 xs

-- | Length testing, but in a less strict way, so that LazySmallCheck can prune the search space.
--
-- @lengthAtLeast xs n == length xs >= n@
lengthAtLeast :: [a] -> Int -> Bool
lengthAtLeast xs     0 = True
lengthAtLeast []     _ = False
lengthAtLeast (x:xs) n = lengthAtLeast xs (n - 1)


data Stream a = a :< Stream a
              deriving (Show)

instance Serial a => Serial (Stream a) where
    series = cons2 (:<)

splitStream :: Int -> Stream a -> ([a], Stream a)
splitStream 0 xs        = ([], xs)
splitStream n (x :< xs) = first (x:) $ splitStream (n - 1) xs

streamPrepend :: [a] -> Stream a -> Stream a
streamPrepend []     ys = ys
streamPrepend (x:xs) ys = x :< streamPrepend xs ys

streamAt :: Stream a -> Int -> a
streamAt (x :< xs) n = if n == 0 then x else streamAt xs (n - 1)

prop_stream :: Stream Int -> Bool
prop_stream xs = (((before ++ [9]) `streamPrepend` after) `streamAt` 4) == 9
  where (before, after) = splitStream 4 xs


data Streem a = Streem a (Stream (Streem a))

instance Serial a => Serial (Streem a) where
    series = cons2 Streem


prop_fun :: (Bool -> Bool) -> Bool
prop_fun f = f True == True && f False == False


main :: IO ()
main = do
    --test prop_stream
    test prop_fun