-- | Day 10 solution
module Day11 (day11) where

import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as M
import Deque.Lazy (Deque)
import qualified Deque.Lazy as D

type MonkeyIndex = Int

type WorryLevel = Integer

data Monkey = Monkey
  { items :: Deque WorryLevel,
    op :: WorryLevel -> WorryLevel,
    throwTo :: WorryLevel -> MonkeyIndex,
    itemsThrown :: Integer
  }

makeMonkey ::
  [WorryLevel] ->
  (WorryLevel -> WorryLevel) ->
  Integer ->
  MonkeyIndex ->
  MonkeyIndex ->
  Monkey
makeMonkey startItems op divisor ifDivisible ifNotDivisible =
  Monkey
    { items = D.fromConsAndSnocLists startItems [],
      op = op,
      throwTo = (\n -> if n `mod` divisor == 0 then ifDivisible else ifNotDivisible),
      itemsThrown = 0
    }

type MonkeyPack = Map MonkeyIndex Monkey

testStartState :: MonkeyPack
testStartState =
  M.fromList
    [ (0, makeMonkey [79, 98] (* 19) 23 2 3),
      (1, makeMonkey [54, 65, 75, 74] (+ 6) 19 2 0),
      (2, makeMonkey [79, 60, 97] (^ 2) 13 1 3),
      (3, makeMonkey [74] (+ 3) 17 0 1)
    ]

realStartState :: MonkeyPack
realStartState =
  M.fromList
    [ (0, makeMonkey [76, 88, 96, 97, 58, 61, 67] (* 19) 3 2 3),
      (1, makeMonkey [93, 71, 79, 83, 69, 70, 94, 98] (+ 8) 11 5 6),
      (2, makeMonkey [50, 74, 67, 92, 61, 76] (* 13) 19 3 1),
      (3, makeMonkey [76, 92] (+ 6) 5 1 6),
      (4, makeMonkey [74, 94, 55, 87, 62] (+ 5) 2 2 0),
      (5, makeMonkey [59, 62, 53, 62] (^ 2) 7 4 7),
      (6, makeMonkey [62] (+ 2) 17 5 7),
      (7, makeMonkey [85, 54, 53] (+ 3) 13 4 0)
    ]

throwFirstItem :: Bool -> Integer -> Monkey -> (Monkey, WorryLevel, MonkeyIndex)
throwFirstItem worried modulus m = (m', wl', mi)
  where
    Just wl = D.head $ items m
    wl' = if worried then op m wl `mod` modulus else op m wl `div` 3
    mi = throwTo m wl'
    m' = m {items = D.tail (items m), itemsThrown = itemsThrown m + 1}

throwAllItems :: Bool -> Integer -> MonkeyPack -> MonkeyIndex -> MonkeyPack
throwAllItems worried modulus mp mi
  | null (items m) = mp
  | otherwise = throwAllItems worried modulus mp' mi
  where
    m = mp M.! mi
    (m', wl', mi') = throwFirstItem worried modulus m
    m'' = mp M.! mi'
    m''' = m'' {items = D.snoc wl' (items m'')}
    mp' = M.insert mi m' $ M.insert mi' m''' mp

throwRound :: Bool -> Integer -> MonkeyPack -> MonkeyPack
throwRound worried modulus mp = foldl (throwAllItems worried modulus) mp $ M.keys mp

monkeyBusinessLevel :: MonkeyPack -> Integer
monkeyBusinessLevel = product . take 2 . sortBy (flip compare) . map itemsThrown . M.elems

day11 :: String -> String
day11 _ =
  unlines
    [ "Test data:",
      show mblTest,
      show mbl2Test,
      "Real data:",
      show mblReal,
      show mbl2Real
    ]
  where
    testModulus = 23 * 19 * 13 * 17
    realModulus = 3 * 11 * 19 * 5 * 2 * 7 * 17 * 13
    mblTest = monkeyBusinessLevel (iterate (throwRound False testModulus) testStartState !! 20)
    mblReal = monkeyBusinessLevel (iterate (throwRound False realModulus) realStartState !! 20)
    mbl2Test = monkeyBusinessLevel (iterate (throwRound True testModulus) testStartState !! 10000)
    mbl2Real = monkeyBusinessLevel (iterate (throwRound True realModulus) realStartState !! 10000)
