{-# LANGUAGE TupleSections #-}

-- | Day 12 solution
module Day12 (day12) where

import Data.Array (Array, indices, listArray, (!))
import Data.Char (ord)
import Data.Graph.Inductive.Basic (grev)
import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.BFS (esp, level)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Tuple (swap)

toElevation :: Char -> (Char, Maybe (), Maybe ())
toElevation 'S' = ('a', Just (), Nothing)
toElevation 'E' = ('z', Nothing, Just ())
toElevation c = (c, Nothing, Nothing)

type Point = (Int, Int)

type Row = Array Int Char

toRow :: String -> (Row, Maybe Int, Maybe Int)
toRow s = (row, maybeStart, maybeEnd)
  where
    tuples = map toElevation s
    row = listArray (0, length tuples - 1) $ map (\(a, _, _) -> a) tuples
    f n = fmap (const n)
    g a@(Just _) _ = a
    g Nothing b = b
    maybeStart = foldl1 g . zipWith f [0 ..] $ map (\(_, b, _) -> b) tuples
    maybeEnd = foldl1 g . zipWith f [0 ..] $ map (\(_, _, c) -> c) tuples

type Grid = Array Int Row

toGrid :: [String] -> (Grid, Point, Point)
toGrid ls = (grid, start, end)
  where
    tuples = map toRow ls
    grid = listArray (0, length tuples - 1) $ map (\(a, _, _) -> a) tuples
    f n = fmap (n,)
    g a@(Just _) _ = a
    g Nothing b = b
    Just start = foldl1 g . zipWith f [0 ..] $ map (\(_, b, _) -> b) tuples
    Just end = foldl1 g . zipWith f [0 ..] $ map (\(_, _, c) -> c) tuples

type NodeMap = Map Point G.Node

nodeMapFromGrid :: Grid -> NodeMap
nodeMapFromGrid g = M.fromList $ zip [(i, j) | i <- indices g, j <- indices (g ! i)] [0 ..]

reverseNodeMap :: NodeMap -> Map G.Node Point
reverseNodeMap = M.fromList . map swap . M.toList

edgesFromGridAndNodeMap :: Grid -> NodeMap -> [G.UEdge]
edgesFromGridAndNodeMap g nm = nsEdges <> snEdges <> weEdges <> ewEdges
  where
    nsEdges =
      [ (nm M.! (i, j), nm M.! (i + 1, j), ())
        | i <- init (indices g),
          j <- indices (g ! i),
          ord (g ! (i + 1) ! j) <= (ord (g ! i ! j)) + 1
      ]
    snEdges =
      [ (nm M.! (i, j), nm M.! (i - 1, j), ())
        | i <- tail (indices g),
          j <- indices (g ! i),
          ord (g ! (i - 1) ! j) <= (ord (g ! i ! j)) + 1
      ]
    weEdges =
      [ (nm M.! (i, j), nm M.! (i, j + 1), ())
        | i <- indices g,
          j <- init (indices (g ! i)),
          ord (g ! i ! (j + 1)) <= (ord (g ! i ! j)) + 1
      ]
    ewEdges =
      [ (nm M.! (i, j), nm M.! (i, j - 1), ())
        | i <- indices g,
          j <- tail (indices (g ! i)),
          ord (g ! i ! (j - 1)) <= (ord (g ! i ! j)) + 1
      ]

type Graph = Gr () ()

graphFromGridAndNodeMap :: Grid -> NodeMap -> Graph
graphFromGridAndNodeMap g nm = G.mkGraph nodes edges
  where
    nodes = map (,()) $ M.elems nm
    edges = edgesFromGridAndNodeMap g nm

day12 :: String -> String
day12 input = unlines $ [show $ length shortestPath - 1, show . snd $ head bfsSearch]
  where
    (grid, start, end) = toGrid $ lines input
    nodeMap = nodeMapFromGrid grid
    graph = graphFromGridAndNodeMap grid nodeMap
    shortestPath = esp (nodeMap M.! start) (nodeMap M.! end) graph
    revGraph = grev graph
    revNodeMap = reverseNodeMap nodeMap
    f (i, j) = (grid ! i ! j) == 'a'
    bfsSearch = filter (f . (revNodeMap M.!) . fst) $ level (nodeMap M.! end) revGraph
