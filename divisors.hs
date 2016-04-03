-- | http://www.codewars.com/kata/544aed4c4a30184e960010f4/train/haskell

module Main where

import Data.List (nub, sort)

divisors :: (Show a, Integral a) => a -> Either String [a]
divisors a = let n = floor . sqrt . fromIntegral $ a
                 ds = sort . nub . concatMap (\i -> [i, div a i]) . filter (\i -> rem a i == 0) $ [2..n] in
             case ds of
               [] -> Left ((show a) ++ " is prime")
               _ -> Right ds

