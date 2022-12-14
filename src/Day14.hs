-- | Day 14 solution
module Day14 (day14) where

import Data.Set (Set)
import qualified Data.Set as S

type Point = (Int, Int)

parsePair :: String -> Point
parsePair w = let (x, y) = break (== ',') w in (read x, read (drop 1 y))

type Wall = [Point]

parseLine :: String -> Wall
parseLine l = f $ words l
  where
    f [] = []
    f (w : ws) = parsePair w : (f $ drop 1 ws)

maximumDepth :: [Wall] -> Int
maximumDepth walls = maximum $ map (maximum . map snd) walls

type CaveMap = Set Point

allRocks :: [Wall] -> CaveMap
allRocks = S.fromList . concatMap f
  where
    f :: Wall -> [Point]
    f [] = error "empty wall"
    f [p] = [p]
    f (p : q : ps) = g p q <> f (q : ps)
    g (x, y) (x', y')
      | x < x' = [(i, y) | i <- [x .. x' -1]]
      | x > x' = [(i, y) | i <- [x' + 1 .. x]]
      | y < y' = [(x, j) | j <- [y .. y' -1]]
      | y > y' = [(x, j) | j <- [y' + 1 .. y]]
      | otherwise = error "identical points"

dropSand :: Int -> CaveMap -> Maybe CaveMap
dropSand maxDepth = f 500 0
  where
    f x y cm
      | y >= maxDepth = Nothing
      | not $ S.member (x, y + 1) cm = f x (y + 1) cm
      | not $ S.member (x -1, y + 1) cm = f (x -1) (y + 1) cm
      | not $ S.member (x + 1, y + 1) cm = f (x + 1) (y + 1) cm
      | otherwise = Just $ S.insert (x, y) cm

dropSand' :: Int -> CaveMap -> Maybe CaveMap
dropSand' maxDepth = f 500 0
  where
    f x y cm
      | S.member (x, y) cm = Nothing
      | y > maxDepth = Just $ S.insert (x, y) cm
      | not $ S.member (x, y + 1) cm = f x (y + 1) cm
      | not $ S.member (x -1, y + 1) cm = f (x -1) (y + 1) cm
      | not $ S.member (x + 1, y + 1) cm = f (x + 1) (y + 1) cm
      | otherwise = Just $ S.insert (x, y) cm

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x =
  x : case f x of
    Just y -> iterateMaybe f y
    Nothing -> []

day14 :: String -> String
day14 input = unlines [show $ length allMaps - 1, show $ length allMaps' - 1]
  where
    walls = map parseLine $ lines input
    maxDepth = maximumDepth walls
    initMap = allRocks walls
    allMaps = iterateMaybe (dropSand maxDepth) initMap
    allMaps' = iterateMaybe (dropSand' maxDepth) initMap
