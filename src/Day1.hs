module Day1 where

import Control.Exception
import System.FilePath ((</>))
import System.IO

type Calories = Int

data Elf = Elf
    { index :: Int
    , caloriesList :: [Int]
    }

instance Eq Elf where
    elf1 == elf2 = totalCalories elf1 == totalCalories elf2

instance Ord Elf where
    compare elf1 elf2 = Prelude.compare (totalCalories elf1) (totalCalories elf2)

totalCalories :: Elf -> Int
totalCalories (Elf _ caloriesList) = sum caloriesList

day1InputFile :: FilePath
day1InputFile = "/Users/ry/advent-of-code/advent-of-code2022/res" </> "day1.in"

day1 :: IO ()
day1 = do
    elfHandle <- System.IO.openFile day1InputFile ReadMode
    elves <- readElves elfHandle
    System.IO.print (totalCalories (maximum elves))

readElves :: Handle -> IO [Elf]
readElves elfHandle = readElves' elfHandle 0

readElves' :: Handle -> Int -> IO [Elf]
readElves' elfHandle index = do
    caloriesList <- readCaloriesList elfHandle
    case caloriesList of
        [] -> return []
        _ -> do
            rest <- readElves' elfHandle (index + 1)
            return (Elf { index, caloriesList } : rest)

readCaloriesList :: Handle -> IO [Int]
readCaloriesList elfHandle = do
    lineEither <- Control.Exception.try (System.IO.hGetLine elfHandle)
    case lineEither of
        Left (_ :: IOError) -> return []
        Right "" -> return []
        Right line -> do
            rest <- readCaloriesList elfHandle
            return ((read line :: Int) : rest)
