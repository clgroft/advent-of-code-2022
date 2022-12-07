-- | Day 7 solution
module Day07 (day07) where

import Data.List (foldl', isPrefixOf, tails)
import Data.Map (Map)
import qualified Data.Map as M

-- Paths are "reversed" so as to make going up a directory easy
type Path = [String]

type FilesystemSizeCount = Map Path Integer

type CountState = (FilesystemSizeCount, Path)

initState :: CountState
initState = (M.empty, [])

updateCountsLine :: FilesystemSizeCount -> Path -> Integer -> FilesystemSizeCount
updateCountsLine fsc p n = foldl' f fsc (tails p)
  where
    f fsc' path = M.insertWith (+) path n fsc'

updateCounts :: CountState -> [String] -> FilesystemSizeCount
updateCounts (fsc, p) = foldl' f fsc
  where
    f :: FilesystemSizeCount -> String -> FilesystemSizeCount
    f fsc' l =
      let w0 = takeWhile (/= ' ') l
       in if w0 == "dir"
            then fsc'
            else updateCountsLine fsc' p (read w0)

getCounts :: [String] -> FilesystemSizeCount
getCounts = f initState
  where
    f (fsc, _) [] = fsc
    f cs@(fsc, p) (l : ls)
      | l == "$ cd /" = f (fsc, []) ls
      | l == "$ cd .." = f (fsc, tail p) ls
      | (w !! 0, w !! 1) == ("$", "cd") = f (fsc, (w !! 2) : p) ls
      | l == "$ ls" = f (updateCounts cs output, p) rest
      | otherwise = error $ "can't parse command " <> l
      where
        w = words l
        (output, rest) = span (not . isPrefixOf "$") ls

day07 :: String -> String
day07 input = unlines [show part1Total, show part2DeleteSize]
  where
    counts = getCounts $ lines input
    sizes = M.elems counts
    part1Total = sum $ filter (<= 100000) sizes
    part2DeleteSize = minimum $ filter (>= (counts M.! [] - 40000000)) sizes
