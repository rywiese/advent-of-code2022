module Day7 where

import IOHelpers
import Data.List.Split
import Data.List (isPrefixOf, sort)
import Data.Foldable (minimumBy)

data Dir = Dir
    { dirName :: String
    , subDirs :: [Dir]
    , files :: [File]
    } deriving (Show)

data File = File
    { fileName :: [Char]
    , fileSize :: Int
    } deriving (Show)

dirSize :: Dir -> Int
dirSize dir = sum (map dirSize (subDirs dir)) + sum (map fileSize (files dir))

flattenDir :: (Dir -> a) -> Dir -> [a]
flattenDir f dir = f dir:foldl (\acc subDir -> flattenDir f subDir ++ acc) [] (subDirs dir)

data Command = LS [File] [String] | CD String deriving (Show)

buildRoot :: [Command] -> Dir
buildRoot = fst . buildDir

buildDir :: [Command] -> (Dir, [Command])
buildDir (CD dirName:LS files subDirNames:CD "..":nextCommands) = (Dir {dirName=dirName, subDirs=[], files=files}, nextCommands)
buildDir (CD dirName:LS files subDirNames:nextCommands) = let (subDirs, remainingCommands) = buildSubDirs nextCommands in
    (Dir {dirName=dirName, subDirs=subDirs, files=files}, remainingCommands)

buildSubDirs :: [Command] -> ([Dir], [Command])
buildSubDirs [] = ([], [])
buildSubDirs (CD "..":remainingCommands) = ([], remainingCommands)
buildSubDirs commands = let (dir, nextCommands) = buildDir commands in
    let (subDirs, remainingCommands) = buildSubDirs nextCommands in (dir:subDirs, remainingCommands)

day7part1 :: IO ()
day7part1 = readAndSolve 7 (sum . filter (<= 100000) . flattenDir dirSize . buildRoot . parseCommands)

parseCommands :: [String] -> [Command]
parseCommands (('$':' ':'c':'d':' ':dirName):t) = CD dirName:parseCommands t
parseCommands ("$ ls":t1) = let
    (lsOutput, t2) = break (isPrefixOf "$") t1 in let
        (files, dirNames) = filesAndDirectoryNames lsOutput in
            LS files dirNames:parseCommands t2
parseCommands _ = []

filesAndDirectoryNames :: [String] -> ([File], [String])
filesAndDirectoryNames [] = ([], [])
filesAndDirectoryNames (('d':'i':'r':' ':dirName):t) = let (files, dirNames) = filesAndDirectoryNames t in (files, dirName:dirNames)
filesAndDirectoryNames (h:t) = let (files, dirNames) = filesAndDirectoryNames t in (fileFromLine h:files, dirNames)

fileFromLine :: String -> File
fileFromLine fileDes = let [sizeString, fileName] = splitOn " " fileDes in (File { fileName=fileName, fileSize=read sizeString })

day7part2 :: IO ()
day7part2 = readAndSolve 7 (feelingLazyToday . buildRoot . parseCommands)

feelingLazyToday :: Dir -> Int
feelingLazyToday root = dirSize . fst . minimumBy (\(_, size1) (_, size2) -> compare size1 size2) . filter (\(_, size) -> size >= spaceToClear root) . flattenDir (\dir -> (dir, dirSize dir)) $ root

totalSpace :: Int
totalSpace = 70000000

spaceNeeded :: Int
spaceNeeded = 30000000

freeSpace :: Dir -> Int
freeSpace root = totalSpace - dirSize root

spaceToClear :: Dir -> Int
spaceToClear root = max 0 (spaceNeeded - freeSpace root)
