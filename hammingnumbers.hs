-- | http://www.codewars.com/kata/526d84b98f428f14a60008da/train/haskell

module Main where

import Data.List (nub)

hamming  :: Int -> Int
hamming n = hammings !! (n-1)

-- refer to https://en.wikipedia.org/wiki/Regular_number
hammings = nub (1:map (*2) hammings `combine` map (*3) hammings `combine` map (*5) hammings)

combine :: [Int] -> [Int] -> [Int]
combine xs [] = xs
combine [] ys = ys
combine (x:xs) (y:ys) = case compare x y of
  LT -> x:combine xs (y:ys)
  GT -> y:combine (x:xs) ys
  EQ -> x: combine xs ys
