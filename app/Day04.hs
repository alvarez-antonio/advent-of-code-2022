module Day04 (part1, part2) where

import Data.List.Split (splitOn)
import Debug.Trace (trace)

toIntRange :: String -> (Int, Int)
toIntRange strRange = do
  let valuesStr = splitOn "-" strRange
  let values = map (read :: String -> Int) valuesStr
  (head values, last values)

containedIn pair1 pair2 =
  fst pair1 <= fst pair2 && snd pair1 >= snd pair2

fullyContains :: [(Int, Int)] -> Bool
fullyContains pairs = do
  let pair1 = head pairs
      pair2 = last pairs
  containedIn pair1 pair2 || containedIn pair2 pair1

overlapping pair1 pair2 =
  containedIn pair2 (fst pair1, fst pair1) || containedIn pair2 (snd pair1, snd pair1)

overlap :: [(Int, Int)] -> Bool
overlap pairs = do
  let pair1 = head pairs
      pair2 = last pairs
  overlapping pair1 pair2 || overlapping pair2 pair1

toPairs fileLines = do
  let pairs = map (splitOn ",") fileLines
  map (map toIntRange) pairs

part1 fileLines = do
  let pairs = toPairs fileLines
      fullyContained = map fullyContains pairs
      countFullyContained = length $ filter (== True) fullyContained
  countFullyContained

part2 fileLines = do
  let pairs = toPairs fileLines
      overlapping = map overlap pairs
  length $ filter (== True) overlapping
