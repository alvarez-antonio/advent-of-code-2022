module Day01 (part1, part2) where

import Data.List (sort)

part1 :: [[Int]] -> Int
part1 convertedSplits = do
  let sums = map sum convertedSplits
  let bigElf = foldr max 0 sums
  bigElf

part2 :: [[Int]] -> Int
part2 convertedSplits = do
  let sums = map sum convertedSplits
  let sorted = reverse (sort sums)
  let final = sum (take 3 sorted)
  final
