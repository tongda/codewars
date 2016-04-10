-- | http://www.codewars.com/kata/53f40dff5f9d31b813000774/train/haskell

module Main where

recoverSecret :: [String] -> String
recoverSecret [] = []
recoverSecret triplets = let c = findHead triplets in c:recoverSecret (removeLetter c triplets)

findHead :: [String] -> Char
findHead xs = head . filter (\c -> isHead c xs) . map head $ xs

isHead :: Char -> [String] -> Bool
isHead c xs = all (\m -> not (c `elem` m && head m /= c)) xs

removeLetter :: Char -> [String] -> [String]
removeLetter c xs = filter (not . null) . map (\s -> filter (/=c) s) $ xs
