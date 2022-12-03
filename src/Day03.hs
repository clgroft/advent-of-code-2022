-- | Day 3 solution
module Day03 (day03) where

import Data.Char (ord)
import Data.List (intersect)
import GHC.Utils.Misc (chunkList)

compartments :: String -> (String, String)
compartments s = (a, b)
  where
    l = length s `div` 2
    a = take l s
    b = drop l s

commonItem :: (String, String) -> Char
commonItem (a, b) = head $ filter (`elem` a) b

priority :: Char -> Int
priority c
  | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
  | otherwise = ord c - ord 'A' + 27

commonItemAll :: [String] -> Char
commonItemAll = head . foldl1 intersect

day03 :: String -> String
day03 input = unlines [show prioritySum, show badgeSum]
  where
    ls = lines input
    prioritySum = sum $ map (priority . commonItem . compartments) ls
    groups = chunkList 3 ls
    badgeSum = sum $ map (priority . commonItemAll) groups
