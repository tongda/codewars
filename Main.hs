-- | http://www.codewars.com/kata/55c4eb777e07c13528000021/train/haskell

module Main where

import qualified Data.Map as Map

zeroes :: (Integral a, Show a) => a -> a -> a
zeroes _ 1 = 0
zeroes b c = minimum . map process . Map.toList . factoring $ b
            where process (x, n) = (allOrders c x) `div` n

allOrders :: Integral a => a -> a -> a
allOrders m n = foldl (\acc x -> acc + orders x n) 0 $ [2..m]
--allOrders m n = sum . map (`orders` n) $ [2..m]

orders :: (Integral a) => a -> a -> a
orders m n = let (q, r) = m `divMod` n in
  case r of
    0 -> 1 + orders q n
    _ -> 0

factoring :: Integral a => a -> Map.Map a a
factoring n = factoringHelper n 2 (Map.fromList [])

factoringHelper :: (Integral a) => a -> a -> Map.Map a a -> Map.Map a a
factoringHelper 1 _ rs = rs
factoringHelper n m rs = let q = n `div` m in
  if n `mod` m == 0 then
                           factoringHelper q 2 (Map.insertWith (+) m 1 rs)
                         else
                           factoringHelper n (m+1) rs

main :: IO ()
main = print (zeroes 10 1000000)
