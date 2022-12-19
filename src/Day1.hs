module Day1 where

import Control.Exception
import System.FilePath ((</>))
import System.IO
import Data.Foldable (maximumBy)

type Elf = Int
type Calories = Int

solution :: IO (Elf, Calories)
solution = do
    elfMap <- readInputFile day1InputFile
    return (findElfWithMostCalories elfMap)

compareElvesCalories:: (Elf, [Calories]) -> (Elf, [Calories]) -> Ordering
compareElvesCalories (_, calories1) (_, calories2) = compare (sum calories1) (sum calories2)

findElfWithMostCalories :: [(Elf, [Calories])] -> (Elf, Calories)
findElfWithMostCalories elfMap = let (elf, caloriesList) = findElfWithMostCalories' elfMap in (elf, sum caloriesList)

findElfWithMostCalories' :: [(Elf, [Calories])] -> (Elf, [Calories])
findElfWithMostCalories' = maximumBy compareElvesCalories

day1InputFile :: FilePath
day1InputFile = "/Users/ry/advent-of-code/advent-of-code2022/res" </> "day1.in"

readInputFile :: FilePath -> IO [(Elf, [Calories])]
readInputFile filePath = do
    elfHandle <- System.IO.openFile filePath ReadMode
    readElfMapList elfHandle 1

readElfMapList :: Handle -> Elf -> IO [(Elf, [Calories])]
readElfMapList elfHandle index = do
    caloriesList <- readCaloriesList elfHandle
    case caloriesList of
        [] -> return []
        _ -> do
            rest <- readElfMapList elfHandle (index + 1)
            return ((index, caloriesList) : rest)

readCaloriesList :: Handle -> IO [Calories]
readCaloriesList elfHandle = do
    lineEither <- Control.Exception.try (System.IO.hGetLine elfHandle)
    case lineEither of
        Left (_ :: IOError) -> return []
        Right "" -> return []
        Right line -> do
            rest <- readCaloriesList elfHandle
            return ((read line :: Int) : rest)
