{-# OPTIONS_GHC -fno-warn-orphans #-}
module Utilities where

import Control.Applicative (Applicative(..))
import Control.Arrow ((***), first, second)
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.ST

import Debug.Trace

import System.IO.Unsafe
import System.Random


instance Applicative (ST s) where
    pure = return
    (<*>) = ap


thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

fth4 :: (a, b, c, d) -> d
fth4 (_, _, _, d) = d

vth5 :: (a, b, c, d, e) -> e
vth5 (_, _, _, _, e) = e

pamf :: Functor f => f a -> (a -> b) -> f b
pamf = flip fmap


{-# NOINLINE exceptionTrace #-}
exceptionTrace :: a -> a
exceptionTrace x = unsafePerformIO (E.evaluate x `E.catch` (\e -> trace ("Exception in pure code: " ++ show (e :: E.SomeException)) $ E.throw e))


genericDeleteAt :: Num i => [a] -> i -> (a, [a])
genericDeleteAt []     _ = error $ "genericDeleteAt: index too large for given list, or list null"
genericDeleteAt (x:xs) n = if n == 0 then (x, xs) else second (x:) (genericDeleteAt xs (n - 1))


newtype Nat = Nat { unNat :: Int }
            deriving (Eq, Ord)

instance Show Nat where
    show = show . unNat

instance Num Nat where
    x + y = Nat (unNat x + unNat y)
    x * y = Nat (unNat x * unNat y)
    x - y | z < 0     = error $ "Subtracting the naturals " ++ show x ++ " and " ++ show y ++ " produced a negative answer"
          | otherwise = Nat z
      where z = unNat x - unNat y
    negate (Nat 0) = Nat 0
    negate x = error $ "Cannot negate the strictly-positive natural number " ++ show x
    abs x = x
    signum (Nat 0) = Nat 0
    signum (Nat _) = Nat 1
    fromInteger x | x < 0     = error $ "The integer " ++ show x ++ " was not a natural number"
                  | otherwise = Nat (fromInteger x)

instance Real Nat where
    toRational = toRational . unNat

instance Enum Nat where
    succ x = Nat (succ (unNat x))
    pred (Nat 0) = error "Cannot take the predecessor of the natural number 0"
    pred x       = Nat (pred (unNat x))
    toEnum x | x < 0     = error $ "Invalid argument to toEnum: " ++ show x
             | otherwise = Nat x
    fromEnum = unNat

instance Integral Nat where
    x `quot` y = Nat (unNat x `quot` unNat y)
    x `rem` y = Nat (unNat x `rem` unNat y)
    x `div` y = Nat (unNat x `div` unNat y)
    x `mod` y = Nat (unNat x `mod` unNat y)
    x `quotRem` y = (Nat *** Nat) (unNat x `quotRem` unNat y)
    x `divMod` y = (Nat *** Nat) (unNat x `divMod` unNat y)
    toInteger = toInteger . unNat

instance Random Nat where
    randomR (lo, hi) g = first Nat $ randomR (unNat lo, unNat hi) g
    random g = first (Nat . abs) $ random g


data Queue a = Queue [a] [a]

emptyQueue :: Queue a
emptyQueue = Queue [] []

queue :: a -> Queue a -> Queue a
queue x (Queue xs ys) = Queue (x : xs) ys

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue xs     (y:ys)) = Just (y, Queue xs ys)
dequeue (Queue []     [])     = Nothing
dequeue (Queue (x:xs) [])     = Just (rev xs x [])
  where
    rev []     x acc = (x, Queue [] acc)
    rev (y:ys) x acc = rev ys y (x:acc)
