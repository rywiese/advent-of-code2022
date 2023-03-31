module Day10 where

import IOHelpers
import Data.List.Split (chunksOf)
import Data.List (sortBy)

everyNthElem :: Int -> [a] -> [a]
everyNthElem n = map head . chunksOf n

everyNthElemStartingWith :: Int -> Int -> [a] -> [a]
everyNthElemStartingWith n startingIndex = everyNthElem n . drop startingIndex

data Instruction = Noop | Addx Int deriving (Show)

cycles :: Instruction -> Int
cycles Noop = 1
cycles (Addx _) = 2

modifyRegisterBy :: Instruction -> (Int -> Int)
modifyRegisterBy Noop = id
modifyRegisterBy (Addx increment) = (+ increment)

registerValuesBefore :: [Instruction] -> [Int]
registerValuesBefore = registerValuesBeforeWithInitialValue 1

registerValuesBeforeWithInitialValue :: Int -> [Instruction] -> [Int]
registerValuesBeforeWithInitialValue _ [] = []
registerValuesBeforeWithInitialValue initialValue (instruction:remainingInstructions) = replicate (cycles instruction) initialValue ++ registerValuesBeforeWithInitialValue (modifyRegisterBy instruction initialValue) remainingInstructions

signalStrengthBefore :: [Instruction] -> [Int]
signalStrengthBefore = zipWith (*) (iterate (+ 1) 1) . registerValuesBefore

-- cycles indexed by zero
interestingSignalStrengths :: [Instruction] -> [Int]
interestingSignalStrengths = everyNthElemStartingWith 40 19 . signalStrengthBefore

sumInterestingSignalStrengths :: [Instruction] -> Int
sumInterestingSignalStrengths = sum . interestingSignalStrengths

day10part1 :: IO ()
day10part1 = parseAndSolve 10 parseInstruction sumInterestingSignalStrengths

screenDimensions :: (Int, Int)
screenDimensions = (40, 6)

type Sprite = (Int, Int, Int)

spriteForRegisterValue :: Int -> Sprite
spriteForRegisterValue x = (x - 1, x, x + 1)

type Pixel = (Int, Int)

data PixelState = Lit | Dark deriving (Show)

pixels :: [Pixel]
pixels = [(x, y) | x <- [0..fst screenDimensions - 1], y <- [0..snd screenDimensions - 1]]

pixelDuring :: [Pixel]
pixelDuring = sortBy (\(x1, y1) (x2, y2) -> case compare y1 y2 of
    EQ -> compare x1 x2
    nonEQ -> nonEQ
    ) pixels

pixelState :: Pixel -> Sprite -> PixelState
pixelState (x, _) (t, u, v) = if x == t || x == u || x == v then Lit else Dark

renderPixelState :: PixelState -> Char
renderPixelState Lit = '#'
renderPixelState Dark = '.'

render :: [PixelState] -> [[Char]]
render = chunksOf (fst screenDimensions) . map renderPixelState

-- Note: to render this nicely, I'm just using regex rather than overriding default "Show" behavior.
-- It may be lazy, but fight me.
-- To read the letters, find: "([#|.]*)",*
--               and replace: $1\n
animate :: [Instruction] -> [[Char]]
animate = render . zipWith pixelState pixelDuring . map spriteForRegisterValue . registerValuesBefore

day10part2 :: IO ()
day10part2 = parseAndSolve 10 parseInstruction animate

parseInstruction :: String -> Instruction
parseInstruction "noop" = Noop
parseInstruction ('a':'d':'d':'x':' ':increment) = Addx (read increment)
