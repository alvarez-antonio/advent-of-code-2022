module Day01 (part1, part2) where

import Data.List (sort)
import Data.List.Split (splitOn)

parseElfInventories fileLines = do
  let inventoriesRaw = splitOn [""] fileLines
  let inventories = map (map read) inventoriesRaw
  inventories

part1 :: [String] -> Int
part1 fileLines = do
  let inventory = parseElfInventories fileLines
  let sums = map sum inventory
  let bigElf = foldr max 0 sums
  bigElf

part2 :: [String] -> Int
part2 fileLines = do
  let inventory = parseElfInventories fileLines
  let sums = map sum inventory
  let sorted = reverse (sort sums)
  let final = sum (take 3 sorted)
  final
