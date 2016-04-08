-- | http://www.codewars.com/kata/5483b69b48cf540cfc000119/train/haskell

module Main where

import Data.Char (chr, ord)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.Function (on)

caesarSort :: [String] -> [[String]]
caesarSort xs = groupBy ((==) `on` uniform) . sortBy (comparing uniform) $ xs

uniform :: String -> String
uniform s@(x:_) = map (\c -> chr (mod (ord c - ord x) 26 + ord 'a')) s
