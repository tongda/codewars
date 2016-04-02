-- | http://www.codewars.com/kata/530045e3c7c0f4d3420001af/train/haskell

module Main where

import Data.List (group)
import Data.Foldable (foldMap)

lookSay :: Integer -> Integer
lookSay = read . foldMap (\s -> (show . length $ s) ++ [head s]) . group . show

