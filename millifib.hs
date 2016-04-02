-- | http://www.codewars.com/kata/53d40c1e2f13e331fc000c26/train/haskell

module Millifib where

fib :: Integer -> Integer
fib n
  | n < 0 = fibIter1 1 0 0 1 (abs n)
  | otherwise = fibIter 1 0 0 1 n

fibIter :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
fibIter _ b _ _ 0 = b
fibIter a b p q n
  | n `mod` 2 == 0 = fibIter a b (p*p + q*q) (q*q + 2*p*q) (n `div` 2)
  | otherwise = fibIter (b*q + a*q + a*p) (b*p + a*q) p q (n-1)

fibIter1 :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
fibIter1 _ b _ _ 0 = b
fibIter1 a b p q n
  | n `mod` 2 == 0 = fibIter a b (q*q - p*p) (-q*q) (n `div` 2)
  | otherwise = fibIter (b*q - a*q - a*p) (b*p + a*q) p q (n-1)
