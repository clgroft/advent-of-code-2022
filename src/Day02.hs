-- | Day 2 solution
module Day02 (day02) where

data Shape = Rock | Paper | Scissors

type Round = (Shape, Shape)

parseTheirShape :: Char -> Shape
parseTheirShape 'A' = Rock
parseTheirShape 'B' = Paper
parseTheirShape 'C' = Scissors
parseTheirShape _ = Rock -- irrelevant

parseMyShape :: Char -> Shape
parseMyShape 'X' = Rock
parseMyShape 'Y' = Paper
parseMyShape 'Z' = Scissors
parseMyShape _ = Rock -- irrelevant

parseRound :: String -> Round
parseRound l = (parseTheirShape (l !! 0), parseMyShape (l !! 2))

parseMyShape2 :: Shape -> Char -> Shape
parseMyShape2 Rock 'X' = Scissors
parseMyShape2 Rock 'Y' = Rock
parseMyShape2 Rock 'Z' = Paper
parseMyShape2 Paper 'X' = Rock
parseMyShape2 Paper 'Y' = Paper
parseMyShape2 Paper 'Z' = Scissors
parseMyShape2 Scissors 'X' = Paper
parseMyShape2 Scissors 'Y' = Scissors
parseMyShape2 Scissors 'Z' = Rock
parseMyShape2 _ _ = Rock -- whatever

parseRound2 :: String -> Round
parseRound2 l = (theirShape, myShape)
  where
    theirShape = parseTheirShape (l !! 0)
    myShape = parseMyShape2 theirShape (l !! 2)

shapeScore :: Shape -> Integer
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

outcomeScore :: Round -> Integer
outcomeScore (Rock, Rock) = 3
outcomeScore (Rock, Paper) = 6
outcomeScore (Rock, Scissors) = 0
outcomeScore (Paper, Rock) = 0
outcomeScore (Paper, Paper) = 3
outcomeScore (Paper, Scissors) = 6
outcomeScore (Scissors, Rock) = 6
outcomeScore (Scissors, Paper) = 0
outcomeScore (Scissors, Scissors) = 3

score :: Round -> Integer
score r@(_, s) = shapeScore s + outcomeScore r

day02 :: String -> String
day02 input = unlines [show naiveScore, show realScore]
  where
    l = lines input
    naiveScore = sum . map (score . parseRound) $ l
    realScore = sum . map (score . parseRound2) $ l
