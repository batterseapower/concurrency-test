import Control.Arrow (first)
import Control.Exception
import Control.Monad
import System.IO.Unsafe

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


-- I used to use unsafeIsEvaluated to decide where to put in "...", but that pruned too heavily because
-- I couldn't show the schedule before it was actually poked on and those thunks turned into real values.
{-# NOINLINE showExplored #-}
showExplored :: (a -> String) -> a -> String
showExplored show x = unsafePerformIO $ fmap (maybe "..." show) $ tryIf isLSCError (evaluate x)
  where
    -- Looked at the LSC code to see what sort of errors it was generating...
    isLSCError (ErrorCall ('\0':msg)) = True
    isLSCError _                      = False

    tryIf :: Exception e => (e -> Bool) -> IO a -> IO (Maybe a)
    tryIf p act = fmap (either (\() -> Nothing) Just) $ tryJust (\e -> guard (p e) >> return ()) act


data Stream a = a :< Stream a

instance Show a => Show (Stream a) where
    show = showExplored $ \(x :< xs) -> show x ++ " :< " ++ show xs

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

prop_stream1 :: Stream Int -> Bool
prop_stream1 xs = (((before ++ [9]) `streamPrepend` after) `streamAt` 4) == 9
  where (before, after) = splitStream 4 xs

prop_stream2 :: Stream Bool -> Bool
prop_stream2 (x :< _) = x == True


data Streem a = Streem a (Stream (Streem a))

instance Serial a => Serial (Streem a) where
    series = cons2 Streem


-- prop_fun :: (Bool -> Bool) -> Bool
-- prop_fun f = f True == True && f False == False


main :: IO ()
main = do
    --test prop_stream1
    test prop_stream2
    --test prop_fun