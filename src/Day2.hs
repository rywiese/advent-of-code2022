module Day2 where

import Data.Proxy
import IOHelpers
import System.FilePath ((</>))
import System.IO

class Scorable a where
    score :: a -> Int

class Scorable a => Interpretation a where
    fromChars :: Char -> Char -> a
    fromString :: String -> a
    fromString line = let [char1, _, char2] = take 3 line in
        fromChars char1 char2

class CharRepresentable a where
    fromChar :: Char -> a

data Shape = Rock | Paper | Scissors

instance Scorable Shape where
    score Rock = 1
    score Paper = 2
    score Scissors = 3

instance CharRepresentable Shape where
    fromChar 'A' = Rock
    fromChar 'X' = Rock
    fromChar 'B' = Paper
    fromChar 'Y' = Paper
    fromChar 'C' = Scissors
    fromChar 'Z' = Scissors

data Outcome = Win | Loss | Draw

instance Scorable Outcome where
    score Win = 6
    score Loss = 0
    score Draw = 3

instance CharRepresentable Outcome where
    fromChar 'X' = Loss
    fromChar 'Y' = Draw
    fromChar 'Z' = Win

type Match = (Shape, Shape)

instance Scorable Match where
    score match = outcomeScore + shapeScore where
        outcomeScore = (score . outcomeOf) match
        shapeScore = let (_, myShape) = match in score myShape

instance Interpretation Match where
    fromChars char1 char2 = (fromChar char1, fromChar char2)

outcomeOf :: Match -> Outcome
outcomeOf match = case match of
    (Rock, Paper) -> Win
    (Paper, Rock) -> Loss
    (Paper, Scissors) -> Win
    (Scissors, Paper) -> Loss
    (Scissors, Rock) -> Win
    (Rock, Scissors) -> Loss
    _ -> Draw

type Playbook = (Shape, Outcome)

shapeToPlay :: Playbook -> Shape
shapeToPlay playbook = case playbook of
    (Rock, Win) -> Paper
    (Rock, Loss) -> Scissors
    (Paper, Win) -> Scissors
    (Paper, Loss) -> Rock
    (Scissors, Win) -> Rock
    (Scissors, Loss) -> Paper
    (opponentShape, Draw) -> opponentShape

matchFrom :: Playbook -> Match
matchFrom playbook = let (shape, _) = playbook in
    (shape, shapeToPlay playbook)

instance Scorable Playbook where
    score = score . matchFrom

instance Interpretation Playbook where
    fromChars char1 char2 = (fromChar char1, fromChar char2)

totalScore :: Interpretation a => [a] -> Int
totalScore = sum . map score

day2InputFile :: FilePath
day2InputFile = "/Users/ry/advent-of-code/advent-of-code2022/res" </> "day2.in"

day2part1 :: IO ()
day2part1 = day2 (Proxy :: Proxy Match)

day2part2 :: IO ()
day2part2 = day2 (Proxy :: Proxy Playbook)

day2 :: forall proxy a. Interpretation a => proxy a -> IO ()
day2 _ = parseAndSolve 2 (fromString :: String -> a) totalScore
