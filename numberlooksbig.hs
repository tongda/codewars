-- | http://www.codewars.com/kata/5287e858c6b5a9678200083c/train/haskell

module Main where

narcissistic :: (Integral n, Show n, Read n) => n -> Bool
narcissistic n = let digits = show n in
  (==n) . sum . map (\x -> (^ length digits) . read $ [x]) $ digits
