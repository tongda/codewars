-- | http://www.codewars.com/kata/55c4eb777e07c13528000021/train/haskell

module Main where

--import Test.Hspec
--import Text.Printf (printf)

import Data.List (sort, group)

zeroes :: Integral a => a -> a -> a
zeroes b c = let fs = factors $ b in
  minimum . map (\(f, n) -> (countFactorialFactor c f) `div` n) $ fs

countFactorialFactor :: Integral a => a -> a -> a
countFactorialFactor m n = sum . map (`countFactor` n) $ [n,n*2..m]

countFactor :: Integral a => a -> a -> a
countFactor m n = if m `mod` n == 0 then 1 + countFactor (m `div` n) n else 0

factors :: Integral a => a -> [(a, a)]
factors = map (\xs -> (head xs, fromIntegral . length $ xs)) . group . sort . factoring

factoring :: Integral a => a -> [a]
factoring n = factoring' n primes

factoring' :: Integral a => a -> [a] -> [a]
factoring' 1 _ = []
factoring' n ps@(p:xs)
  | n `mod` p == 0 = p: factoring' (n `div` p) ps
  | otherwise = factoring' n xs

primes :: Integral a => [a]
primes = 2:filterPrimes [3,5..]
  where filterPrimes (x:xs) = x: filterPrimes [y | y <- xs, y `mod` x /= 0]

testZeroes :: Integral a => a -> a -> a -> Spec
testZeroes b n z =
  it (printf "%i! in the base %i should have %i zeroes"
             (toInteger n) (toInteger b) (toInteger z)) $
    toInteger (zeroes b n) `shouldBe` toInteger z

main = hspec $ do
  describe "factoring" $ do
    it "should give factors" $ do
      factoring 2 `shouldBe` [2]
      factoring 6 `shouldBe` [2,3]
      factoring 16 `shouldBe` [2,2,2,2]
      factoring 19 `shouldBe` [19]
  describe "count factorial factors" $ do
    it "should count factors" $ do
      countFactorialFactor 4 2 `shouldBe` 3
      countFactorialFactor 16 2 `shouldBe` 15
  describe "base cases" $ do
    testZeroes 10 10 2
    testZeroes 16 16 3
    testZeroes 19 18 0
    testZeroes 10 600000 149998
    testZeroes 10 1000000 249998
