-- | http://www.codewars.com/kata/51b6249c4612257ac0000005/train/haskell

module Main where

import Test.Hspec

solution :: String -> Int
solution [] = 0
solution ('M':xs) = 1000 + solution xs
solution ('C':'M':xs) = 900 + solution xs
solution ('D':xs) = 500 + solution xs
solution ('C':'D':xs) = 400 + solution xs
solution ('C':xs) = 100 + solution xs
solution ('X':'C':xs) = 90 + solution xs
solution ('L':xs) = 50 + solution xs
solution ('X':'L':xs) = 40 + solution xs
solution ('X':xs) = 10 + solution xs
solution ('I':'X':xs) = 9 + solution xs
solution ('V':xs) = 5 + solution xs
solution ('I':'V':xs) = 4 + solution xs
solution ('I':xs) = 1 + solution xs

main = hspec $ do
  describe "solution" $ do
    it "should solve one digit number" $ do
      solution "M" `shouldBe` 1000
    it "should solve multiple digit number" $ do
      solution "XXI" `shouldBe` 21
      solution "MCMXC" `shouldBe` 1990
