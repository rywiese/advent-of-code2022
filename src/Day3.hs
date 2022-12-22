module Day3 where

import Data.Char
import Data.List
import Data.List.Split
import IOHelpers

type ItemType = Char

data RuckSack = RuckSack
    { compartment1 :: [ItemType]
    , compartment2 :: [ItemType]
    }

items :: RuckSack -> [ItemType]
items rucksack = compartment1 rucksack ++ compartment2 rucksack

firstCommonElement :: Eq a => [a] -> [a] -> a
firstCommonElement l1 l2 = head (Data.List.intersect l1 l2)

commonElement :: RuckSack -> ItemType
commonElement RuckSack { compartment1 = c1, compartment2 = c2 } = firstCommonElement c1 c2

findBadge :: RuckSack -> RuckSack -> RuckSack -> ItemType
findBadge r1 r2 r3 = head (items r1 `intersect` items r2 `intersect` items r3)

findBadgeUncurried :: (RuckSack, RuckSack, RuckSack) -> ItemType
findBadgeUncurried (r1, r2, r3) = findBadge r1 r2 r3

priorityOf :: ItemType -> Int
priorityOf itemType = let ord = Data.Char.ord itemType in
    if ord < 97 then ord - 38 else ord - 96

ruckSackFromString :: String -> RuckSack
ruckSackFromString string = let [compartment1, compartment2] = Data.List.Split.chunksOf ((Data.List.length string) `div` 2) string in
    RuckSack { compartment1, compartment2 }

totalPrioritiesOfCommonElements :: [RuckSack] -> Int
totalPrioritiesOfCommonElements = sum . map (priorityOf . commonElement)

chunksOf3 :: [a] -> [(a, a, a)]
chunksOf3 = map (\[a1, a2, a3] -> (a1, a2, a3)) . chunksOf 3

totalPrioritiesOfBadges :: [RuckSack] -> Int
totalPrioritiesOfBadges = sum . map (priorityOf . findBadgeUncurried) . chunksOf3

day3part1 :: IO ()
day3part1 = parseAndSolve 3 ruckSackFromString totalPrioritiesOfCommonElements

day3part2 :: IO ()
day3part2 = parseAndSolve 3 ruckSackFromString totalPrioritiesOfBadges
