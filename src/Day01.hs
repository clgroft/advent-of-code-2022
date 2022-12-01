-- | Day 1 solution
module Day01 (day01) where

import Data.List (sortBy)

breakIntoParagraphs :: [String] -> [[String]]
breakIntoParagraphs [] = []
breakIntoParagraphs lineList = paragraph : breakIntoParagraphs rest
  where
    (paragraph, remainder) = break null lineList
    rest = dropWhile null remainder

makeCalorieList :: [String] -> [Integer]
makeCalorieList = map read

day01 :: String -> String
day01 input = unlines [show maxCalories, show topThreeCalories]
  where
    totalCalories = map (sum . makeCalorieList) . breakIntoParagraphs $ lines input
    maxCalories = maximum totalCalories
    topThreeCalories = sum . take 3 $ sortBy (flip compare) totalCalories
