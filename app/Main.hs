module Main where

import Data.List.Split (splitOn)
import Day01
import Day02
import System.Environment (getArgs)
import Text.Printf

solution :: Int -> Int -> [String] -> Int
solution 1 1 = Day01.part1
solution 1 2 = Day01.part2
solution 2 1 = Day02.part1
solution 2 2 = Day02.part2

main :: IO ()
main = do
  args <- getArgs
  let day = read (head args)
      part = read (last args)
  fileStr <- readFile (printf "app/inputs/Day%02d-input" day)
  let result = solution day part (lines fileStr)

  print result
