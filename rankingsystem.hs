-- | http://www.codewars.com/kata/51fda2d95d6efda45e00004e/train/haskell

module Main where

import Test.Hspec

data User = User { rank :: Int, progress :: Int}

ranks :: [Int]
ranks = [-8..(-1)] ++ [1..8]

updateRank :: Int -> Int -> Int
updateRank m n = case drop n . dropWhile (/=m) $ ranks of
  [] -> last ranks
  (x:xs) -> x

diffRank :: Int -> Int -> Int
diffRank m n = if m < n then diff m n else negate (diff n m) where
  diff x y = ((-) 1) . length . dropWhile (/=y) . reverse . dropWhile (/=x) $ ranks

newUser :: User
newUser = User {rank = -8, progress = 0}

incProgress :: Int -> User -> User
incProgress n (User r p)
  | n > 8 || n < -8 || n == 0 = error "invalid"
  | otherwise = User { rank = newRank, progress = newProgress} where
      newProgress = if newRank == last ranks then 0 else (p + points) `rem` 100
      newRank = updateRank r ((p + points) `div` 100)
      points = case diffRank n r of
        -1 -> 1
        0 -> 3
        i | i > 0 -> 10 * i * i
        _ -> 0

main :: IO ()
main = hspec $ do
  describe "diff rank" $ do
    it "should return correct diff" $ do
      diffRank (-8) (-8) `shouldBe` 0
      diffRank (-1) 1 `shouldBe` -1
      diffRank 1 (-1) `shouldBe` 1
      diffRank (-4) (-8) `shouldBe` 4
  describe "new user" $ do
    it "should return -8 level of new user" $ do
      rank newUser `shouldBe` -8
  describe "inc progress" $ do
    it "should add correct point of delta level" $ do
      (progress . incProgress (-8) $ newUser) `shouldBe` 3
      (progress . incProgress (-7) $ newUser) `shouldBe` 10
    it "should level up" $ do
      (progress . incProgress (-4) $ newUser) `shouldBe` 60
      (rank . incProgress (-4) $ newUser) `shouldBe` (-7)
      (progress . incProgress 1 $ User (-1) 0) `shouldBe` 10
      (progress . incProgress 2 $ User (-1) 80) `shouldBe` 20
      (rank . incProgress 2 $ User (-1) 80) `shouldBe` 1

