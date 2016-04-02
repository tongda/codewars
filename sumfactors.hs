-- | http://www.codewars.com/kata/54d496788776e49e6b00052f/train/haskell

module Main where

import Control.Monad
import Data.List

sumOfDivided :: [Integer] -> [(Integer, Integer)]
sumOfDivided xs = let m = fromIntegral . (+50) . (`div` 2) . maximum . map abs $ xs in do
--  p <- primes m
  p <- take m primesPE
  guard . any (\x -> x `mod` p == 0) $ xs
  return (p, sum . filter (\x -> rem x p == 0) $ xs)

primes :: Integer -> [Integer]
primes m = 2:primeGen [3,5..m] where
  primeGen [] = []
  primeGen (x:xs) = x:primeGen (xs `minus` [x*x,x*x+x..])

minus :: [Integer] -> [Integer] -> [Integer]
minus (x:xs) (y:ys) = case (compare x y) of
  LT -> x:minus xs (y:ys)
  EQ -> minus xs ys
  GT -> minus (x:xs) ys
minus xs _ = xs

primesPE :: [Integer]
primesPE = 2 : oddprimes
  where
    oddprimes = sieve [3,5..] 9 oddprimes
    sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t

main :: IO ()
main = print . sumOfDivided $ ([1..2000] ++ [100000])
