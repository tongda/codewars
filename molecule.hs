-- | http://www.codewars.com/kata/52f831fa9d332c6591000511/train/haskell

module Molecule where

import Data.Char
import qualified Data.Map as M

type MoleCount = Maybe Int

value :: MoleCount -> Int
value Nothing = 1
value (Just i) = i

plus :: MoleCount -> MoleCount -> MoleCount
plus a b = Just (value a + value b)

mul :: Int -> MoleCount -> MoleCount
mul i m = Just ((value m) * i)

combine :: MoleCount -> Int -> MoleCount
combine Nothing i = Just i
combine (Just n) i = Just (10 * n + i)

data MoleNode = LeafNode (String, MoleCount) | BranchNode [MoleNode] MoleCount | SymbolNode String deriving (Show)

isSymbolNode :: String -> MoleNode -> Bool
isSymbolNode sym (SymbolNode sym') = if sym == sym' then True else False
isSymbolNode _ _ = False

push :: Char -> [MoleNode] -> Maybe [MoleNode]
push '[' ns = Just (SymbolNode "[":ns)
push '(' ns = Just (SymbolNode "(":ns)
push '{' ns = Just (SymbolNode "{":ns)
push ']' ns = let (ans, bns) = break (isSymbolNode "[") ns in
  case bns of
    [] -> Nothing
    _:bbns -> Just (BranchNode ans Nothing:bbns)
push ')' ns = let (ans, bns) = break (isSymbolNode "(") ns in
  case bns of
    [] -> Nothing
    _:bbns -> Just (BranchNode ans Nothing:bbns)
push '}' ns = let (ans, bns) = break (isSymbolNode "{") ns in
  case bns of
    [] -> Nothing
    _:bbns -> Just (BranchNode ans Nothing:bbns)
push x []
  | isUpper x = Just [LeafNode ([x], Nothing)]
  | otherwise = Nothing
push x nodes@(LeafNode (y, n):ns)
  | isLower x = if isUpper . head $ y then Just (LeafNode (y++[x], n):ns) else Nothing
  | isDigit x = Just (LeafNode (y, combine n (read [x])):ns)
  | otherwise = Just (LeafNode ([x], Nothing):nodes)
push x nodes@(SymbolNode _:_)
  | isUpper x = Just (LeafNode ([x], Nothing):nodes)
  | otherwise = Nothing
push x nodes@(BranchNode nns n:ns)
  | isDigit x = Just (BranchNode nns (combine n (read [x])):ns)
  | isUpper x = Just (LeafNode ([x], Nothing):nodes)
  | otherwise = Nothing

buildNodes :: String -> Maybe [MoleNode]
buildNodes = foldl (\nodes ch -> nodes >>= push ch) (Just [])

validate :: [MoleNode] -> Maybe [MoleNode]
validate ns = if any (isSymbolNode "[") ns || any (isSymbolNode "(") ns then Nothing else Just ns

buildMap :: [MoleNode] -> M.Map String MoleCount
buildMap ns = let process mp (LeafNode (m,n)) = M.insertWith plus m n mp
                  process mp (BranchNode nns n) = M.unionWith plus (M.map (mul . value $ n) (buildMap nns)) mp in
              foldl process (M.fromList []) ns

parseMolecule :: String -> Either String [(String,Int)]
parseMolecule input = case fmap buildMap ((buildNodes input) >>= validate) of
  Nothing -> Left "Not a valid molecule"
  Just mp -> Right (map (\(m, n) -> (m, value n)) . M.toList $ mp)
