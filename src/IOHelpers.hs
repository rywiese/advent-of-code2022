module IOHelpers where

import Control.Exception
import System.FilePath ((</>))
import System.IO

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
