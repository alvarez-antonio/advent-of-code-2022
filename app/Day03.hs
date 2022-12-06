module Day03 (part1, part2) where

import Data.Char (ord)
import Data.List (intersect, nub)
import Data.List.Split (chunksOf)

priorityOf :: Char -> Int
priorityOf item
  | ord item >= ord 'a' = ord item - ord 'a' + 1
  | ord item >= ord 'A' = ord item - ord 'A' + 27

halfpoint x = length x `div` 2

splitInHalf x = splitAt (halfpoint x) x

part1 fileLines = do
  let containers = map splitInHalf fileLines
  let commonItems = map (uncurry intersect) containers
  let uniqueCommonItems = map nub commonItems
  let prioritiesChar = map last uniqueCommonItems
  let priorities = map priorityOf prioritiesChar
  sum priorities

part2 fileLines = do
  let groups = chunksOf 3 fileLines
  let badges = map (foldr1 intersect) groups
  let uniqueBadges = map nub badges
  let badgesChar = map last uniqueBadges
  let priorities = map priorityOf badgesChar
  sum priorities