module Day9 where

import IOHelpers
import Data.Set (fromList)

data Direction = C | N | NE | E | SE | S | SW | W | NW deriving (Show)

data HeadMovement = U | D | L | R deriving (Show)

type TailMovement = Direction

type TailPosition = Direction

type Coordinates = (Int, Int)

follow :: TailPosition -> HeadMovement -> (Maybe TailMovement, TailPosition)
follow C U = (Nothing, S)
follow C D = (Nothing, N)
follow C L = (Nothing, E)
follow C R = (Nothing, W)
follow N U = (Nothing, C)
follow N D = (Just S, N)
follow N L = (Nothing, NE)
follow N R = (Nothing, NW)
follow NE U = (Nothing, E)
follow NE D = (Just SW, N)
follow NE L = (Just SW, E)
follow NE R = (Nothing, N)
follow E U = (Nothing, SE)
follow E D = (Nothing, NE)
follow E L = (Just W, E)
follow E R = (Nothing, C)
follow SE U = (Just NW, S)
follow SE D = (Nothing, E)
follow SE L = (Just NW, E)
follow SE R = (Nothing, S)
follow S U = (Just N, S)
follow S D = (Nothing, C)
follow S L = (Nothing, SE)
follow S R = (Nothing, SW)
follow SW U = (Just NE, S)
follow SW D = (Nothing, W)
follow SW L = (Nothing, S)
follow SW R = (Just NE, W)
follow W U = (Nothing, SW)
follow W D = (Nothing, NW)
follow W L = (Nothing, C)
follow W R = (Just E, W)
follow NW U = (Nothing, W)
follow NW D = (Just SE, N)
follow NW L = (Nothing, N)
follow NW R = (Just SE, W)

tailMovements :: [HeadMovement] -> [TailMovement]
tailMovements = reverse . snd . foldl accumulateTailMovements (C, [])

accumulateTailMovements :: (TailPosition, [TailMovement]) -> HeadMovement -> (TailPosition, [TailMovement])
accumulateTailMovements (lastTailPosition, tailMovementsSoFar) headMovement = case follow lastTailPosition headMovement of
    (Nothing, newTailPosition) -> (newTailPosition, tailMovementsSoFar)
    (Just tailMovement, newTailPosition) -> (newTailPosition, tailMovement:tailMovementsSoFar)

newCoordinatesFor :: Coordinates -> TailMovement -> Coordinates
newCoordinatesFor (x, y) C = (x, y)
newCoordinatesFor (x, y) N = (x, y + 1)
newCoordinatesFor (x, y) NE = (x + 1, y + 1)
newCoordinatesFor (x, y) E = (x + 1, y)
newCoordinatesFor (x, y) SE = (x + 1, y - 1)
newCoordinatesFor (x, y) S = (x, y - 1)
newCoordinatesFor (x, y) SW = (x - 1, y - 1)
newCoordinatesFor (x, y) W = (x -1 , y)
newCoordinatesFor (x, y) NW = (x - 1, y + 1)

appendNewCoordinatesFor :: [Coordinates] -> TailMovement -> [Coordinates]
appendNewCoordinatesFor [] _ = []
appendNewCoordinatesFor (h:t) tailMovement = newCoordinatesFor h tailMovement:h:t

buildCoordinatesFrom :: [TailMovement] -> [Coordinates]
buildCoordinatesFrom = foldl appendNewCoordinatesFor [(0, 0)]

numUniqueCoordinates :: [HeadMovement] -> Int
numUniqueCoordinates = length . fromList . buildCoordinatesFrom . tailMovements

day9part1 :: IO ()
day9part1 = readAndSolve 9 (numUniqueCoordinates . parseSteps)

data Instruction = Instruction
    { headMovement :: HeadMovement
    , steps :: Int
    } deriving (Show)

parseSteps :: [String] -> [HeadMovement]
parseSteps = concatMap (expandSteps . parseInstruction)

expandSteps :: Instruction -> [HeadMovement]
expandSteps instruction = replicate (steps instruction) (headMovement instruction)

parseInstruction :: String -> Instruction
parseInstruction (headMovement:' ':steps) = Instruction { headMovement = parseHeadMovement headMovement, steps = read steps}

parseHeadMovement :: Char -> HeadMovement
parseHeadMovement 'U' = U
parseHeadMovement 'D' = D
parseHeadMovement 'L' = L
parseHeadMovement 'R' = R
