-- | Day 8 solution
module Day08 (day08) where

import Data.Array (Array, bounds, listArray, (!))

type TreeGrid = Array Int (Array Int Char)

lineOfTrees :: String -> Array Int Char
lineOfTrees s = listArray (0, length s - 1) s

gridOfTrees :: [String] -> TreeGrid
gridOfTrees ls = listArray (0, length ls - 1) $ map lineOfTrees ls

visibleFromNorth :: TreeGrid -> Char -> Int -> Int -> Bool
visibleFromNorth grid h r c = and [(grid ! r' ! c) < h | r' <- [0 .. r - 1]]

visibleFromSouth :: TreeGrid -> Char -> Int -> Int -> Bool
visibleFromSouth grid h r c = and [(grid ! r' ! c) < h | r' <- [r + 1 .. lastRow]]
  where
    (_, lastRow) = bounds grid

visibleFromWest :: TreeGrid -> Char -> Int -> Int -> Bool
visibleFromWest grid h r c = and [row ! c' < h | c' <- [0 .. c - 1]]
  where
    row = grid ! r

visibleFromEast :: TreeGrid -> Char -> Int -> Int -> Bool
visibleFromEast grid h r c = and [row ! c' < h | c' <- [c + 1 .. lastCol]]
  where
    row = grid ! r
    (_, lastCol) = bounds row

tallTrees :: TreeGrid -> [(Int, Int)]
tallTrees grid =
  [ (r, c) | r <- [0 .. lastRow], c <- [0 .. lastCol], let h = grid ! r ! c, visibleFromNorth grid h r c || visibleFromSouth grid h r c || visibleFromWest grid h r c || visibleFromEast grid h r c
  ]
  where
    (_, lastRow) = bounds grid
    (_, lastCol) = bounds (grid ! 0)

scoreNorth :: TreeGrid -> Char -> Int -> Int -> Int
scoreNorth grid h r c = length prefix + (if null suffix then 0 else 1)
  where
    (prefix, suffix) = span (< h) [grid ! r' ! c | r' <- [r - 1, r - 2 .. 0]]

scoreSouth :: TreeGrid -> Char -> Int -> Int -> Int
scoreSouth grid h r c = length prefix + (if null suffix then 0 else 1)
  where
    (_, lastRow) = bounds grid
    (prefix, suffix) = span (< h) [grid ! r' ! c | r' <- [r + 1 .. lastRow]]

scoreWest :: TreeGrid -> Char -> Int -> Int -> Int
scoreWest grid h r c = length prefix + (if null suffix then 0 else 1)
  where
    row = grid ! r
    (prefix, suffix) = span (< h) [row ! c' | c' <- [c - 1, c - 2 .. 0]]

scoreEast :: TreeGrid -> Char -> Int -> Int -> Int
scoreEast grid h r c = length prefix + (if null suffix then 0 else 1)
  where
    row = grid ! r
    (_, lastCol) = bounds row
    (prefix, suffix) = span (< h) [row ! c' | c' <- [c + 1 .. lastCol]]

score :: TreeGrid -> Int -> Int -> Int
score grid r c = product [scoreNorth grid h r c, scoreSouth grid h r c, scoreWest grid h r c, scoreEast grid h r c]
  where
    h = grid ! r ! c

day08 :: String -> String
day08 input = unlines [show tallTreeCount, show maxScore]
  where
    grid = gridOfTrees $ lines input
    tallTreeCount = length $ tallTrees grid
    (_, lastRow) = bounds grid
    (_, lastCol) = bounds (grid ! 0)
    maxScore = maximum [score grid r c | r <- [0 .. lastRow], c <- [0 .. lastCol]]
