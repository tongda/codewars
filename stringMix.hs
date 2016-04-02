-- | http://www.codewars.com/kata/5629db57620258aa9d000014/train/haskell

module Mix where

import Data.Char
import Data.List
import Data.Monoid
import Data.Ord
import qualified Data.Map as M

mix :: [Char] -> [Char] -> [Char]
mix s1 s2 = let m1 = M.fromList . map (\s -> (head s, ("1", s))) . breakString $ s1
                m2 = M.fromList . map (\s -> (head s, ("2", s))) . breakString $ s2
                m = M.unionWith (\p1@(_, p1s) p2@(_,p2s) -> case compare (length p1s) (length p2s) of
                                                                 GT -> p1
                                                                 LT -> p2
                                                                 EQ -> ("=", p1s)) m1 m2
                es = M.elems m
                sortedElems = (Down (length p1s) `compare` Down (length p2s)) `mappend` (sg1 `compare` sg2) `mappend` (head p1s `compare` head p2s)) $ es in
            intercalate "/" . map (\(sign, str) -> sign ++ ":" ++ str) $ sortedElems

breakString :: String -> [String]
breakString str = filter ((>1) . length) . reverse . sortBy (comparing length) . group . reverse . sort . filter isLower $ str
