module Day06 (part1, part2) where

import Data.List (nub)
import Debug.Trace (trace)
import GHC.IO.Handle (NewlineMode (inputNL))

indexOfMarker :: Int -> String -> Int -> Int
indexOfMarker criteria input index = do
  let firstFour = take criteria input
      unique = nub firstFour
  if length unique == criteria
    then trace ("Unique: " ++ show unique ++ " Index: " ++ show index) index + criteria
    else trace ("Unique: " ++ show unique ++ " Index: " ++ show index) indexOfMarker criteria (tail input) (index + 1)

part1 :: [String] -> Int
part1 fileLines =
  indexOfMarker 4 (head fileLines) 0

part2 fileLines =
  indexOfMarker 14 (head fileLines) 0