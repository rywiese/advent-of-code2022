module Day8 where

import IOHelpers
import Data.Char (digitToInt)
import Data.Foldable (maximumBy)

crossSection :: Int -> [[a]] -> [a]
crossSection index = map (!! index)

quadToList :: (a, a, a, a) -> [a]
quadToList (w, x, y, z) = [w, x, y, z]

data Tree = Tree
    { row :: Int
    , column :: Int
    , height :: Int
    } deriving (Show)

instance Eq Tree where
    tree1 == tree2 = height tree1 == height tree2

instance Ord Tree where
    tree1 <= tree2 = height tree1 <= height tree2

neighbors :: [[Tree]] -> Tree -> ([Tree], [Tree], [Tree], [Tree])
neighbors forest tree = let (upNeighbors, _:downNeighbors) = splitAt (row tree) (crossSection (column tree) forest) in
    let (leftNeighbors, _:rightNeighbors) = splitAt (column tree) (forest !! row tree) in
        (reverse upNeighbors, downNeighbors, reverse leftNeighbors, rightNeighbors)

visibleInForest :: [[Tree]] -> Tree -> Bool
visibleInForest forest tree = foldl (\acc -> (acc ||) . all (< tree)) False $ quadToList $ neighbors forest tree

numberOfTreesVisible :: [[Tree]] -> Int
numberOfTreesVisible forest = (length . filter (visibleInForest forest) . concat) forest

day8part1 :: IO ()
day8part1 = readAndSolve 8 (numberOfTreesVisible . parseForest)

viewingDistance :: Tree -> [Tree] -> Int
viewingDistance _ [] = 0
viewingDistance tree (h:t) = if height tree <= height h then 1 else 1 + viewingDistance tree t

scenicScore :: [[Tree]] -> Tree -> Int
scenicScore forest tree = foldl (\acc neighbs -> acc * viewingDistance tree neighbs) 1 (quadToList $ neighbors forest tree)

treeWithMaxScenicScore :: [[Tree]] -> Tree
treeWithMaxScenicScore forest = (maximumBy (\tree1 tree2 -> compare (scenicScore forest tree1) (scenicScore forest tree2)) . concat) forest

maxScenicScore :: [[Tree]] -> Int
maxScenicScore forest = scenicScore forest $ treeWithMaxScenicScore forest

day8part2 :: IO ()
day8part2 = readAndSolve 8 (maxScenicScore . parseForest)

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
