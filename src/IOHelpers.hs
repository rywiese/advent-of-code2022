module IOHelpers where

import Control.Exception
import System.FilePath ((</>))
import System.IO

-- parseAndSolve probably should be written in terms of readAndSolve, not the other way around
-- TODO: add tests for previous days to ensure no regressions (and to learn Haskell tests :)),
-- then refactor at will
readAndSolve :: Show a => Int -> ([String] -> a) -> IO ()
readAndSolve day = parseAndSolve day id

parseAndSolve :: Show b => Int -> (String -> a) -> ([a] -> b) -> IO ()
parseAndSolve day lineParser solve = do
    handle <- System.IO.openFile (inputFileForDay day) ReadMode
    parsedLines <- collectLinesBy lineParser handle
    System.IO.print (solve parsedLines)

collectLinesBy :: (String -> a) -> Handle -> IO [a]
collectLinesBy f handle = do
    lineEither :: Either IOError String <- Control.Exception.try (System.IO.hGetLine handle)
    case lineEither of
        Right line -> do
            rest <- collectLinesBy f handle
            return (f line : rest)
        _ -> return []

inputFileForDay :: Int -> FilePath
inputFileForDay day = "/Users/ry/advent-of-code/advent-of-code2022/res" </> "day" ++ show day ++ ".in"
