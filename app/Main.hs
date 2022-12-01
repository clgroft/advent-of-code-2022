module Main where

import Data.Array ((!))
import MyLib (solutions)
import System.Environment (getArgs)

main :: IO ()
main = do
  (hd : _) <- getArgs
  interact $ solutions ! read hd
