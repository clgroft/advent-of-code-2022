-- | Day 5 solution
module Day05 (day05) where

import Data.Map (Map, (!))
import qualified Data.Map as M

breakIntoParagraphs :: [String] -> [[String]]
breakIntoParagraphs [] = []
breakIntoParagraphs lineList = paragraph : breakIntoParagraphs rest
  where
    (paragraph, remainder) = break null lineList
    rest = dropWhile null remainder

type State = Map Int String

initialState :: [String] -> State
initialState = M.fromList . zip [1 ..]

data Instruction = Instruction
  { numCrates :: Int,
    source :: Int,
    destination :: Int
  }

parseInstruction :: String -> Instruction
parseInstruction s = Instruction (read (ws !! 1)) (read (ws !! 3)) (read (ws !! 5))
  where
    ws = words s

executeInstruction :: State -> Instruction -> State
executeInstruction s (Instruction n srcIx dstIx) = s''
  where
    src = s ! srcIx
    dst = s ! dstIx
    s' = M.insert srcIx (drop n src) s
    s'' = M.insert dstIx ((reverse $ take n src) <> dst) s'

executeAllInstructions :: State -> [Instruction] -> State
executeAllInstructions = foldl executeInstruction

executeInstruction' :: State -> Instruction -> State
executeInstruction' s (Instruction n srcIx dstIx) = s''
  where
    src = s ! srcIx
    dst = s ! dstIx
    s' = M.insert srcIx (drop n src) s
    s'' = M.insert dstIx (take n src <> dst) s'

executeAllInstructions' :: State -> [Instruction] -> State
executeAllInstructions' = foldl executeInstruction'

day05 :: String -> String
day05 input = unlines [topLine, topLine']
  where
    [initStateStrs, instrStrs] = breakIntoParagraphs $ lines input
    initState = initialState initStateStrs
    instrs = map parseInstruction instrStrs
    finalState = executeAllInstructions initState instrs
    topLine = map (head . snd) $ M.toList finalState
    finalState' = executeAllInstructions' initState instrs
    topLine' = map (head . snd) $ M.toList finalState'
