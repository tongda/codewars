-- | http://www.codewars.com/kata/55e7280b40e1c4a06d0000aa/train/haskell

module Main where

import Data.List (sort)

chooseBestSum :: Int -> Int -> [Int] -> Maybe Int
chooseBestSum _ _ [] = Nothing
chooseBestSum t 1 ls = case dropWhile (>t) . reverse . sort $ ls of
  [] -> Nothing
  xs -> Just (head xs)
chooseBestSum t k (x:xs)
  | t <= 0 = Nothing
  | otherwise = max' (chooseBestSum t k xs) ((+) <$> Just x <*> chooseBestSum (t-x) (k-1) xs)

max' :: Maybe Int -> Maybe Int -> Maybe Int
max' x Nothing = x
max' Nothing x = x
max' (Just x) (Just y) = Just (max x y)

