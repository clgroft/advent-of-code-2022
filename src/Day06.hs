-- | Day 6 solution
module Day06 (day06) where

import Data.List (tails)

redundant :: (Eq a) => [a] -> Bool
redundant [] = False
redundant (x : xs) = x `elem` xs || redundant xs

position :: Int -> String -> Int
position n s = n + (length . takeWhile redundant . map (take n) $ tails s)

day06 :: String -> String
day06 input = unlines [show $ position 4 input, show $ position 14 input]
