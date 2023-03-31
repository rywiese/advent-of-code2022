module Day9 where

import IOHelpers
import Data.Set (fromList)

composeNTimes :: (a -> a) -> Int -> (a -> a)
composeNTimes f n = foldl (.) id (replicate n f)

data Direction = C | N | NE | E | SE | S | SW | W | NW deriving (Show)

type HeadMovement = Direction

type TailMovement = Direction

type TailPosition = Direction

type Coordinates = (Int, Int)

follow :: TailPosition -> HeadMovement -> (Maybe TailMovement, TailPosition)

follow C C = (Nothing, C)
follow C N = (Nothing, S)
follow C NE = (Nothing, SW)
follow C E = (Nothing, W)
follow C SE = (Nothing, NW)
follow C S = (Nothing, N)
follow C SW = (Nothing, NE)
follow C W = (Nothing, E)
follow C NW = (Nothing, SE)

follow N C = (Nothing, N)
follow N N = (Nothing, C)
follow N NE = (Nothing, W)
follow N E = (Nothing, NW)
follow N SE = (Just SE, N)
follow N S = (Just S, N)
follow N SW = (Just SW, N)
follow N W = (Nothing, NE)
follow N NW = (Nothing, E)

follow NE C = (Nothing, NE)
follow NE N = (Nothing, E)
follow NE NE = (Nothing, C)
follow NE E = (Nothing, N)
follow NE SE = (Just S, N)
follow NE S = (Just SW, N)
follow NE SW = (Just SW, NE)
follow NE W = (Just SW, E)
follow NE NW = (Just W, E)

follow E C = (Nothing, E)
follow E N = (Nothing, SE)
follow E NE = (Nothing, S)
follow E E = (Nothing, C)
follow E SE = (Nothing, N)
follow E S = (Nothing, NE)
follow E SW = (Just SW, E)
follow E W = (Just W, E)
follow E NW = (Just NW, E)

follow SE C = (Nothing, SE)
follow SE N = (Just NW, S)
follow SE NE = (Just N, S)
follow SE E = (Nothing, S)
follow SE SE = (Nothing, C)
follow SE S = (Nothing, E)
follow SE SW = (Just W, E)
follow SE W = (Just NW, E)
follow SE NW = (Just NW, SE)

follow S C = (Nothing, S)
follow S N = (Just N, S)
follow S NE = (Just NE, S)
follow S E = (Nothing, SW)
follow S SE = (Nothing, W)
follow S S = (Nothing, C)
follow S SW = (Nothing, E)
follow S W = (Nothing, SE)
follow S NW = (Just NW, S)

follow SW C = (Nothing, SW)
follow SW N = (Just NE, S)
follow SW NE = (Just NE, SW)
follow SW E = (Just NE, W)
follow SW SE = (Just E, W)
follow SW S = (Nothing, W)
follow SW SW = (Nothing, C)
follow SW W = (Nothing, S)
follow SW NW = (Just N, S)

follow W C = (Nothing, W)
follow W N = (Nothing, SW)
follow W NE = (Just NE, W)
follow W E = (Just E, W)
follow W SE = (Just SE, W)
follow W S = (Nothing, NW)
follow W SW = (Nothing, N)
follow W W = (Nothing, C)
follow W NW = (Nothing, S)

follow NW C = (Nothing, NW)
follow NW N = (Nothing, W)
follow NW NE = (Just E, W)
follow NW E = (Just SE, W)
follow NW SE = (Just SE, NW)
follow NW S = (Just SE, N)
follow NW SW = (Just S, N)
follow NW W = (Nothing, N)
follow NW NW = (Nothing, C)

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
newCoordinatesFor (x, y) W = (x - 1 , y)
newCoordinatesFor (x, y) NW = (x - 1, y + 1)

appendNewCoordinatesFor :: [Coordinates] -> TailMovement -> [Coordinates]
appendNewCoordinatesFor [] _ = []
appendNewCoordinatesFor (h:t) tailMovement = newCoordinatesFor h tailMovement:h:t

buildCoordinatesFrom :: [TailMovement] -> [Coordinates]
buildCoordinatesFrom = foldl appendNewCoordinatesFor [(0, 0)]

numUniqueCoordinates :: Int -> [HeadMovement] -> Int
numUniqueCoordinates numKnots = length . fromList . buildCoordinatesFrom . composeNTimes tailMovements (numKnots - 1)

day9part1 :: IO ()
day9part1 = readAndSolve 9 (numUniqueCoordinates 2 . parseSteps)

day9part2 :: IO ()
day9part2 = readAndSolve 9 (numUniqueCoordinates 10 . parseSteps)

data Instruction = Instruction
    { headMovement :: HeadMovement
    , steps :: Int
    } deriving (Show)

parseSteps :: [String] -> [HeadMovement]
parseSteps = concatMap (expandSteps . parseInstruction)

expandSteps :: Instruction -> [HeadMovement]
expandSteps instruction = replicate (steps instruction) (headMovement instruction)

parseInstruction :: String -> Instruction
parseInstruction (headMovement:' ':steps) = Instruction { headMovement = parseHeadMovement headMovement, steps = read steps }

parseHeadMovement :: Char -> HeadMovement
parseHeadMovement 'U' = N
parseHeadMovement 'D' = S
parseHeadMovement 'L' = W
parseHeadMovement 'R' = E
