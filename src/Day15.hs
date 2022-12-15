-- | Day 15 solution
module Day15 (day15) where

import qualified Data.Set as S

type Point = (Integer, Integer) -- (x,y)

type Interval = (Integer, Integer) -- closed interval

parseRow :: String -> Integer
parseRow = read . drop 2

grabInteger :: String -> (Integer, String)
grabInteger s = let (nstr, s') = span (`elem` '-' : ['0' .. '9']) s in (read nstr, s')

parseSensorBeacon :: String -> (Point, Point)
parseSensorBeacon s = ((x, y), (x', y'))
  where
    s1 = drop (length "Sensor at x=") s
    (x, s2) = grabInteger s1
    s3 = drop (length ", y=") s2
    (y, s4) = grabInteger s3
    s5 = drop (length ": closest beacon is at x=") s4
    (x', s6) = grabInteger s5
    s7 = drop (length ", y=") s6
    (y', _) = grabInteger s7

distance :: Point -> Point -> Integer
distance (a, b) (c, d) = abs (c - a) + abs (d - b)

intervalInRow :: Integer -> (Point, Point) -> [Interval]
intervalInRow y'' (p@(x, y), q) = if pm < 0 then [] else [(x - pm, x + pm)]
  where
    dist = distance p q
    pm = dist - abs (y'' - y)

addInterval :: Interval -> [Interval] -> [Interval]
addInterval i [] = [i]
addInterval (a, b) ((c, d) : is)
  | b < c - 1 = (a, b) : (c, d) : is
  | d < a - 1 = (c, d) : addInterval (a, b) is
  | otherwise = addInterval (a `min` c, b `max` d) is

len :: Interval -> Integer
len (a, b) = b - a + 1

data Bounds = Bounds
  { pmin :: Integer,
    pmax :: Integer,
    mmin :: Integer,
    mmax :: Integer
  }
  deriving (Show)

noBounds :: Bounds
noBounds = Bounds 0 8000000 (-4000000) 4000000

xpyMin, xpyMax, xmyMin, xmyMax :: Point -> Integer -> Bounds
xpyMin (a, b) dist = noBounds {pmin = a + b + dist + 1}
xpyMax (a, b) dist = noBounds {pmax = a + b - dist - 1}
xmyMin (a, b) dist = noBounds {mmin = a - b + dist + 1}
xmyMax (a, b) dist = noBounds {mmax = a - b - dist - 1}

possibles :: Point -> Integer -> [Bounds]
possibles p dist = [xpyMin p dist, xpyMax p dist, xmyMin p dist, xmyMax p dist]

intersectBounds :: Bounds -> Bounds -> [Bounds]
intersectBounds bds bds' =
  let bds'' = Bounds (pmin bds `max` pmin bds') (pmax bds `min` pmax bds') (mmin bds `max` mmin bds') (mmax bds `min` mmax bds')
   in if pmin bds'' <= pmax bds'' && mmin bds'' <= mmax bds'' then [bds''] else []

intersectPossibleBounds :: [Bounds] -> [Bounds] -> [Bounds]
intersectPossibleBounds as bs = [c | a <- as, b <- bs, c <- intersectBounds a b]

day15 :: String -> String
day15 input = unlines [show cellsWithNoBeacons, show (4000000 * x' + y')]
  where
    ls = lines input
    y = parseRow $ head ls
    sensorBeacons = map parseSensorBeacon $ tail ls
    beaconsInRow = S.fromList . map fst . filter ((== y) . snd) $ map snd sensorBeacons
    rawIntervals = concatMap (intervalInRow y) sensorBeacons
    combinedIntervals = foldr addInterval [] rawIntervals
    cellsWithNoBeacons = (fromInteger . sum $ map len combinedIntervals) - S.size beaconsInRow
    separatedBounds = map (\(p, q) -> possibles p (distance p q)) sensorBeacons
    combinedBounds = foldl intersectPossibleBounds [noBounds] separatedBounds
    validBounds = head $ filter (\bds -> pmin bds == pmax bds && mmin bds == mmax bds) combinedBounds
    x' = (pmin validBounds + mmin validBounds) `div` 2
    y' = (pmin validBounds - mmin validBounds) `div` 2
