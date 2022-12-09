-- | Day 9 solution
module Day09 (day09) where

import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as S

type Position = (Int, Int)

data RopeState = RopeState
  { rope :: [Position], -- head of list is tail of rope
    visited :: Set Position
  }

startState :: Int -> RopeState
startState n = RopeState (replicate n (0, 0)) $ S.singleton (0, 0)

moveRope :: [Position] -> String -> [Position]
moveRope [] _ = error "empty rope"
moveRope [(r, c)] "U" = [(r - 1, c)]
moveRope [(r, c)] "D" = [(r + 1, c)]
moveRope [(r, c)] "L" = [(r, c - 1)]
moveRope [(r, c)] "R" = [(r, c + 1)]
moveRope [p] dir = error $ "Unrecognized direction: " <> dir
moveRope (p : ps) dir = reconcile (head ps') p : ps'
  where
    ps' = moveRope ps dir

reconcile :: Position -> Position -> Position
reconcile (r, c) (r', c')
  | abs (r' - r) <= 1 && abs (c' - c) <= 1 = (r', c')
  | r' == r && c' == c - 2 = (r', c' + 1)
  | r' == r && c' == c + 2 = (r', c' - 1)
  | c' == c && r' == r - 2 = (r' + 1, c')
  | c' == c && r' == r + 2 = (r' - 1, c')
  | r' < r && c' < c = (r' + 1, c' + 1)
  | r' < r && c' > c = (r' + 1, c' - 1)
  | r' > r && c' < c = (r' - 1, c' + 1)
  | r' > r && c' > c = (r' - 1, c' - 1)
  | otherwise = error "illegal rope state"

moveHead :: RopeState -> String -> RopeState
moveHead (RopeState ps visited) dir = RopeState ps' (S.insert (head ps') visited)
  where
    ps' = moveRope ps dir

moveSequence :: [String] -> [String]
moveSequence = concatMap f
  where
    f l = replicate (read w1) w0
      where
        [w0, w1] = words l

day09 :: String -> String
day09 input = unlines [show numVisited, show numVisited2]
  where
    moves = moveSequence $ lines input
    numVisited = S.size . visited $ foldl' moveHead (startState 2) moves
    numVisited2 = S.size . visited $ foldl' moveHead (startState 10) moves
