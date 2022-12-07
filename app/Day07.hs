module Day07 (part1, part2) where

import Data.Function (on)
import Data.HashMap.Strict (HashMap, empty, findWithDefault, insert, insertWith, lookup, toList)
import Data.List (find, intercalate, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (trace)
import Foreign (free)

run :: String -> HashMap String Int -> [String] -> (HashMap String Int, [String])
run "$ cd .." directories currentDir = (directories, init currentDir)
run "$ ls" directories currentDir = (directories, currentDir)
run command directories currentDir = (directories, currentDir ++ [drop 5 command])

toPath ["/"] = "/"
toPath currentDir = "/" ++ intercalate "/" (tail currentDir)

updateAllDirectorySizes directories [] size = directories
updateAllDirectorySizes directories currentDir size =
  updateAllDirectorySizes (insertWith (+) (toPath currentDir) size directories) (init currentDir) size

updateDirectories :: HashMap String Int -> [String] -> String -> (HashMap String Int, [String])
updateDirectories directories currentDir terminalOutput = do
  let fileSize = (read :: String -> Int) $ head $ splitOn " " terminalOutput
      updatedDirectories = updateAllDirectorySizes directories currentDir fileSize
  trace ("Current: " ++ show currentDir ++ "\nCommand: " ++ show terminalOutput ++ "\nDirectories: " ++ show (toList updatedDirectories)) (updatedDirectories, currentDir)

switch terminalOutput directories currentDir
  | head terminalOutput == '$' = run terminalOutput directories currentDir
  | take 4 terminalOutput == "dir " = (directories, currentDir)
  | otherwise = updateDirectories directories currentDir terminalOutput

process :: [String] -> HashMap String Int -> [String] -> HashMap String Int
process [] directories currentDir = directories
process terminalOutputs directories currentDir = do
  let (updatedDirectories, updatedCurrentDir) = switch (head terminalOutputs) directories currentDir
  process (tail terminalOutputs) updatedDirectories updatedCurrentDir

part1 fileLines = do
  let directories = toList $ process fileLines empty []
      smallDirs = filter (\x -> snd x <= 100000) directories
      totalSize = trace ("Small Directories: " ++ show (sortBy (compare `on` fst) smallDirs)) sum $ map snd smallDirs
  totalSize

part2 fileLines = do
  let directories = process fileLines empty []
      totalSize = Data.HashMap.Strict.lookup "/" directories
      sortedSizes = sort $ map snd (toList $ directories)
      freeSpace = 70000000 - sum totalSize
  trace ("Free: " ++ show freeSpace) fromJust $ find (> 30000000 - freeSpace) sortedSizes