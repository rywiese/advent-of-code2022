module Day8 where

import IOHelpers
import Data.Char (digitToInt)

crossSection :: Int -> [[a]] -> [a]
crossSection index = map (!! index)

data Tree = Tree
    { row :: Int
    , column :: Int
    , height :: Int
    } deriving (Show)

instance Eq Tree where
    tree1 == tree2 = height tree1 == height tree2

instance Ord Tree where
    tree1 <= tree2 = height tree1 <= height tree2

visibleInGrid :: [[Tree]] -> Tree -> Bool
visibleInGrid forest tree = visibleInRow (forest !! row tree) (column tree) || visibleInRow (crossSection (column tree) forest) (row tree)

visibleInRow :: [Tree] -> Int -> Bool
visibleInRow trees index = case splitAt index trees of
    ([], _) -> True
    (_, []) -> False
    (_, [_]) -> True
    (left, h:right) -> all (< h) left || all (< h) right

numberOfTreesVisible :: [[Tree]] -> Int
numberOfTreesVisible grid = (length . filter (visibleInGrid grid) . concat) grid

day8part1 :: IO ()
day8part1 = readAndSolve 8 (numberOfTreesVisible . parseForest)

day8part2 :: IO ()
day8part2 = readAndSolve 8 id

parseForest :: [String] -> [[Tree]]
parseForest = heightGridToForest . parseHeightGrid

heightGridToForest :: [[Int]] -> [[Tree]]
heightGridToForest = heightGridToForest' 0

heightGridToForest' :: Int -> [[Int]] -> [[Tree]]
heightGridToForest' _ [] = []
heightGridToForest' rowIndex (heightRow:rest) = heightRowToTreeRow rowIndex heightRow:heightGridToForest' (rowIndex + 1) rest

heightRowToTreeRow :: Int -> [Int] -> [Tree]
heightRowToTreeRow rowIndex = heightRowToTreeRow' rowIndex 0

heightRowToTreeRow' :: Int -> Int -> [Int] -> [Tree]
heightRowToTreeRow' _ _ [] = []
heightRowToTreeRow' rowIndex columnIndex (height:rest) = Tree {row=rowIndex, column=columnIndex, height=height}:heightRowToTreeRow' rowIndex (columnIndex + 1) rest

parseHeightGrid :: [String] -> [[Int]]
parseHeightGrid = (map . map) digitToInt
