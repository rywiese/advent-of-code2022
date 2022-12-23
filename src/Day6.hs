module Day6 where

import IOHelpers
import Data.List (nub)

day6part1 :: IO ()
day6part1 = parseAndSolve 6 id (firstMarkerAfterNUniqueChars 4 . head)

day6part2 :: IO ()
day6part2 = parseAndSolve 6 id (firstMarkerAfterNUniqueChars 14 . head)

firstMarkerAfterNUniqueChars :: Int -> String -> Int
firstMarkerAfterNUniqueChars n string = let firstN = take n string in
    if nub firstN == firstN then n else 1 + firstMarkerAfterNUniqueChars n (drop 1 string)
