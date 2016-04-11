-- | http://www.codewars.com/kata/526d84b98f428f14a60008da/train/haskell

module Main where

import Data.List (nub)

hamming  :: Int -> Int
hamming n = head . drop (n-1) $ hammings2

-- refer to https://en.wikipedia.org/wiki/Regular_number
hammings2 = nub (1:combine (combine (map (*2) hammings2) (map (*3) hammings2)) (map (*5) hammings2))

combine :: [Int] -> [Int] -> [Int]
combine xs [] = xs
combine [] ys = ys
combine (x:xs) (y:ys) = case compare x y of
  LT -> x:combine xs (y:ys)
  GT -> y:combine (x:xs) ys
  EQ -> x: combine xs ys
