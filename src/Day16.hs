-- | Day 16 solution
-- | Largely copied from github.com/glguy/advent/blob/main/solutions/src/2022/16.hs
module Day16 (day16) where

import Data.List (tails)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Valve = String

data ValveInfo = ValveInfo
  { flowRate :: Int,
    connectedValves :: [Valve]
  }
  deriving (Show)

type AllValveInfo = Map Valve ValveInfo

type Parser = Parsec Void String

parseValve :: Parser Valve
parseValve = (\a b -> [a, b]) <$> upperChar <*> upperChar

parseValveList :: Parser [Valve]
parseValveList = (:) <$> parseValve <*> (string ", " *> parseValveList <|> return [])

toValveInfo :: Parser (Valve, ValveInfo)
toValveInfo = do
  _ <- string "Valve "
  sourceValve <- parseValve
  _ <- string " has flow rate="
  fr <- read <$> many digitChar
  _ <- string "; tunnel"
  _ <- string "s" <|> return ""
  _ <- string " lead"
  _ <- string "s" <|> return ""
  _ <- string " to valve"
  _ <- string "s" <|> return ""
  _ <- string " "
  cv <- parseValveList
  return (sourceValve, ValveInfo fr cv)

solver :: AllValveInfo -> Int -> Map (Set String) Int
solver avi = go [(("AA", S.empty), 0)]
  where
    go states 0 = M.fromListWith max [(open, n) | ((_, open), n) <- states]
    go states t = go (simplify (concatMap step states)) (t - 1)
      where
        step ((here, open), n) =
          [((next, open), n) | next <- connectedValves (avi M.! here)]
            <> [ ((here, S.insert here open), n + (t -1) * amt)
                 | S.notMember here open,
                   let amt = flowRate (avi M.! here),
                   amt /= 0
               ]
        simplify = M.assocs . M.fromListWith max

day16 :: String -> String
day16 input = unlines [show $ maximum solutions1, show maximum2]
  where
    valveInfoList = map (fromJust . parseMaybe toValveInfo) $ lines input
    valveInfo = M.fromList valveInfoList
    solutions1 = solver valveInfo 30
    solutions2 = solver valveInfo 26
    maximum2 =
      maximum
        [ v1 + v2
          | (open1, v1) : elephants <- tails (M.assocs solutions2),
            (open2, v2) <- elephants,
            S.null (S.intersection open1 open2)
        ]
