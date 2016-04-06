-- | http://www.codewars.com/kata/53223653a191940f2b000877/train/haskell

module Main where

import Test.Hspec

import Data.List (nub, sort)

type Node = Char
type Arc  = (Node, Node)

solveGraph :: Node -> Node -> [Arc] -> Bool
solveGraph s e arcs = if s == e && s `elem` map fst arcs
                      then True
                      else solveGraph' [s] e arcs

solveGraph' :: [Node] -> Node -> [Arc] -> Bool
solveGraph' ns e arcs = if ns == nns
                        then False
                        else if e `elem` nns
                             then True
                             else solveGraph' nns e arcs
  where nns = nextNodes ns arcs

nextNodes :: [Node] -> [Arc] -> [Node]
nextNodes ns arcs = sort . nub . (++ns) . concatMap (\n -> map snd . filter (\(x, _) -> n == x) $ arcs) $ ns

main = hspec $ do
  describe "simple graph with 1 arc" $ do
    let arcs = [('a', 'b')]

    it "should reach a" $ solveGraph 'a' 'a' arcs `shouldBe` True
    it "should reach b" $ solveGraph 'a' 'b' arcs `shouldBe` True
    it "should never reach c" $ solveGraph 'a' 'c' arcs `shouldBe` False
    it "should never start b" $ solveGraph 'b' 'c' arcs `shouldBe` False
  describe "complex graph with loops and intermediary nodes" $ do
    let arcs = [('a','b'),('b','c'),('c','a'),('c','d'),('e','a')]
    it "should reach d" $ solveGraph 'a' 'd' arcs `shouldBe` True
    it "should never reach nodes with no arcs leading to it" $
      solveGraph 'a' 'e' arcs `shouldBe` False
    it "should reach all nodes in a loop" $ do
      solveGraph 'a' 'a' arcs `shouldBe` True
      solveGraph 'a' 'b' arcs `shouldBe` True
      solveGraph 'a' 'c' arcs `shouldBe` True

