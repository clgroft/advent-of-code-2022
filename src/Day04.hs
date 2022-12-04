-- | Day 4 solution
module Day04 (day04) where

type Pair = (Int, Int)

split :: Char -> String -> [String]
split _ "" = []
split c s = let (w, r) = break (== c) s in w : split c (dropWhile (== c) r)

parsePair :: String -> Pair
parsePair s = (a, b) where [a, b] = map read $ split '-' s

parseTwoPairs :: String -> (Pair, Pair)
parseTwoPairs s = (a, b) where [a, b] = map parsePair $ split ',' s

isContainedIn :: Pair -> Pair -> Bool
isContainedIn (a, b) (c, d) = a >= c && b <= d

isRedundant :: (Pair, Pair) -> Bool
isRedundant (p, q) = p `isContainedIn` q || q `isContainedIn` p

overlaps :: (Pair, Pair) -> Bool
overlaps ((a, b), (c, d)) = not (b < c || d < a)

day04 :: String -> String
day04 input = unlines [show redundantCount, show overlapCount]
  where
    pairPairs = map parseTwoPairs $ lines input
    redundantCount = length $ filter isRedundant pairPairs
    overlapCount = length $ filter overlaps pairPairs
