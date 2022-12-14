module MyLib
  ( solutions,
  )
where

import Data.Array
  ( Array,
    listArray,
  )
import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Day05 (day05)
import Day06 (day06)
import Day07 (day07)
import Day08 (day08)
import Day09 (day09)
import Day10 (day10)
import Day11 (day11)
import Day12 (day12)
import Day13 (day13)
import Day14 (day14)
import Day15 (day15)
import Day16 (day16)

solutions :: Array Int (String -> String)
solutions =
  listArray
    (1, 25)
    [ day01,
      day02,
      day03,
      day04,
      day05,
      day06,
      day07,
      day08,
      day09,
      day10,
      day11,
      day12,
      day13,
      day14,
      day15,
      day16
    ]
