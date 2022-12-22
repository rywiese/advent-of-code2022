module Day4 where

import Data.List.Split
import IOHelpers
import Prelude

type SectionId = Int

data SectionAssignment = SectionAssignment
    { mini :: SectionId
    , maxi :: SectionId
    }

leftFullyContainsRight :: SectionAssignment -> SectionAssignment -> Bool
leftFullyContainsRight
    SectionAssignment { mini=min1, maxi=max1 }
    SectionAssignment { mini=min2, maxi=max2 } = min1 <= min2 && max1 >= max2

oneFullyContainsTheOther :: SectionAssignment -> SectionAssignment -> Bool
oneFullyContainsTheOther ass1 ass2 = leftFullyContainsRight ass1 ass2 || leftFullyContainsRight ass2 ass1

hasOverlap :: SectionAssignment -> SectionAssignment -> Bool
hasOverlap
    SectionAssignment { mini=min1, maxi=max1 }
    SectionAssignment { mini=min2, maxi=max2 } = min2 <= max1 && min1 <= max2

parseLine :: String -> (SectionAssignment, SectionAssignment)
parseLine line = let [min1, max1, min2, max2] = map read (splitOneOf "-," line) in
    (SectionAssignment min1 max1, SectionAssignment min2 max2)

numPairsWhere :: (a -> a -> Bool) -> [(a, a)] -> Int
numPairsWhere f = length . filter id . map (uncurry f)

day4part1 :: IO ()
day4part1 = parseAndSolve 4 parseLine (numPairsWhere oneFullyContainsTheOther)

day4part2 :: IO ()
day4part2 = parseAndSolve 4 parseLine (numPairsWhere hasOverlap)
