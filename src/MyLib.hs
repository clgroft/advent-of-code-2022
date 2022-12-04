module MyLib (solutions) where

import Data.Array (Array, listArray)
import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)

solutions :: Array Int (String -> String)
solutions = listArray (1, 25) [day01, day02, day03, day04]
