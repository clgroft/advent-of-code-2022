-- | Day 12 solution
module Day13 (day13) where

import Data.List (sort)
import Text.Read (Read)

data Packet = Single Int | Bundle [Packet] deriving (Eq)

instance (Show Packet) where
  show (Single n) = show n
  show (Bundle ps) = show ps

instance (Read Packet) where
  readsPrec k s
    | head s == '[' = map (\(ps, s') -> (Bundle ps, s')) $ readsPrec k s
    | otherwise = map (\(n, s') -> (Single n, s')) $ readsPrec k s

instance (Ord Packet) where
  Single n <= Single m = n <= m
  Bundle ps <= Bundle qs = ps <= qs
  Single n <= Bundle ps = [Single n] <= ps
  Bundle ps <= Single n = ps <= [Single n]

parseLines :: [String] -> [(Packet, Packet)]
parseLines [] = []
parseLines ls = (p, q) : parseLines ls'
  where
    [p, q] = map read $ take 2 ls
    ls' = drop 3 ls

isOrdered :: (Packet, Packet) -> Bool
isOrdered = uncurry (<)

dividerPackets :: [Packet]
dividerPackets = [Bundle [Bundle [Single 2]], Bundle [Bundle [Single 6]]]

day13 :: String -> String
day13 input = unlines [show sumIndices, show productIndices]
  where
    packetPairs = parseLines $ lines input
    sumIndices = sum . map fst . filter (isOrdered . snd) $ zip [1 ..] packetPairs
    allPackets = sort $ dividerPackets <> concatMap (\(p, q) -> [p, q]) packetPairs
    indexedPackets = zip [1 ..] allPackets
    productIndices = product . map fst $ filter ((`elem` dividerPackets) . snd) indexedPackets
