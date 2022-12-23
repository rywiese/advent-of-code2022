module Day5 where

import Data.List.Index
import Data.List.Split
import IOHelpers
import Prelude
import Data.Maybe (listToMaybe)

type Crate = Char

type Column = Int

-- head is top
type Stack = [Crate]

type CrossSection = [Maybe Crate]

data Instruction = Instruction
    { quantity :: Int
    , source :: Column
    , dest :: Column
    } deriving (Show)

data Problem = Problem
    { stacks :: [Stack]
    , instructions :: [Instruction]
    } deriving (Show)

day5part1 :: IO ()
day5part1 = parseAndSolve 5 id solve

solve :: [String] -> [Maybe Crate]
solve = applyAndGetTops . parseProblem

applyAndGetTops :: Problem -> [Maybe Crate]
applyAndGetTops = map listToMaybe . apply

apply :: Problem -> [Stack]
apply (Problem { stacks=stackies, instructions=[] }) = stackies
apply (Problem { stacks=stackies, instructions=firstInstruction:t }) = apply
    (Problem { stacks=moveCratePer firstInstruction stackies, instructions=t})

moveCratePer :: Instruction -> [Stack] -> [Stack]
moveCratePer (Instruction { quantity=q, source=s, dest=d }) stacks =
    let (ss, sd) = moveCratesFromTo q (stacks !! (s - 1)) (stacks !! (d - 1)) in
        (setAt (d - 1) sd . setAt (s - 1) ss) stacks

moveCratesFromTo :: Int -> Stack -> Stack -> (Stack, Stack)
moveCratesFromTo n s1 s2 = if n <= 0
    then (s1, s2)
    else let (s1', s2') = moveCrateFromTo s1 s2 in
        moveCratesFromTo (n - 1) s1' s2'

moveCrateFromTo :: Stack -> Stack -> (Stack, Stack)
moveCrateFromTo s1 s2 = case s1 of
    [] -> (s1, s2)
    h:t -> (t, h:s2)

parseProblem :: [String] -> Problem
parseProblem lines = Problem { stacks=stackies, instructions=instructies } where
    stackies = (parseStacks . takeWhile isCrossSection) lines
    instructies = (parseInstructions . dropWhile (not . isInstruction)) lines

isCrossSection :: String -> Bool
isCrossSection ('[':_) = True
isCrossSection _ = False

isInstruction :: String -> Bool
isInstruction ('m':_) = True
isInstruction _ = False

parseStacks :: [String] -> [Stack]
parseStacks = transposeStacks . parseCrossSections

transposeStacks :: [CrossSection] -> [Stack]
transposeStacks = foldr spreadOverTop [[], [], [], [], [], [], [], [], []]

spreadOverTop :: CrossSection -> [Stack] -> [Stack]
spreadOverTop = zipWith stackOnTop

stackOnTop :: Maybe Crate -> Stack -> Stack
stackOnTop Nothing = id
stackOnTop (Just crate) = (:) crate

parseCrossSections :: [String] -> [CrossSection]
parseCrossSections = map parseCrossSection

parseCrossSection :: String -> CrossSection
parseCrossSection = map parseCrate . divvy 3 4

parseCrate :: String -> Maybe Crate
parseCrate ['[', crate, ']'] = Just crate
parseCrate _ = Nothing

parseInstructions :: [String] -> [Instruction]
parseInstructions = map parseInstruction

parseInstruction :: String -> Instruction
parseInstruction string = Instruction {quantity=q, source=s, dest=d} where
    [q, s, d] = map read $ filter (/= "") $ splitOneOf "movefromto " string
