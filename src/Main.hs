module Main where

import Data.Char
import System.IO (hFlush, stdout)
import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= parse >>= runDG . parseDG

parse ["-h"] = usage        >> exit
parse ["-v"] = version      >> exit
parse []     = usage        >> exitWith (ExitFailure 1)
parse fs     = concat <$> mapM readFile fs

usage   = putStrLn "Usage: digits [-v, -h] <file>"
version = putStrLn "digits version 1.0.0"
exit    = exitWith ExitSuccess

data DGCommand = MoveRight     -- 0
               | MoveLeft      -- 1
               | Increment     -- 2
               | Decrement     -- 3
               | Print         -- 4
               | Read          -- 5
               | LoopL         -- 6
               | LoopR         -- 7
               | Comment Char  -- anything else

type DGSource = [DGCommand]

parseDG :: String -> DGSource
parseDG = map charToDG
    where
        charToDG x = case x of
            '0' -> MoveRight
            '1' -> MoveLeft
            '2' -> Increment
            '3' -> Decrement
            '4' -> Print
            '5' -> Read
            '6' -> LoopL
            '7' -> LoopR
            c   -> Comment c


data Tape a = Tape [a] -- Left of the pivot element
                    a  -- Pivot element
                   [a] -- Right of the pivot element

emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
    where zeros = repeat 0

moveRight :: Tape a -> Tape a
moveRight (Tape ls p (r:rs)) = Tape (p:ls) r rs

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) p rs) = Tape ls l (p:rs)

runDG :: DGSource -> IO ()
runDG = run emptyTape . bfSource2Tape
    where bfSource2Tape (b:bs) = Tape [] b bs

advance :: Tape Int
    -> Tape DGCommand
    -> IO ()

advance dataTape (Tape _ _ []) = return ()
advance dataTape source = run dataTape (moveRight source)

seekLoopR :: Int
          -> Tape Int
          -> Tape DGCommand
          -> IO ()
seekLoopR 1 dataTape source@(Tape _ LoopR _) = advance dataTape source
seekLoopR b dataTape source@(Tape _ LoopR _) =
    seekLoopR (b-1) dataTape (moveRight source)
seekLoopR b dataTape source@(Tape _ LoopL _) =
    seekLoopR (b+1) dataTape (moveRight source)
seekLoopR b dataTape source =
    seekLoopR b dataTape (moveRight source)

seekLoopL :: Int
          -> Tape Int
          -> Tape DGCommand
          -> IO ()
seekLoopL 1 dataTape source@(Tape _ LoopL _) = advance dataTape source
seekLoopL b dataTape source@(Tape _ LoopL _) =
    seekLoopL (b-1) dataTape (moveLeft source)
seekLoopL b dataTape source@(Tape _ LoopR _) =
    seekLoopL (b+1) dataTape (moveLeft source)
seekLoopL b dataTape source =
    seekLoopL b dataTape (moveLeft source)

run :: Tape Int
    -> Tape DGCommand
    -> IO ()

run dataTape source@(Tape _ MoveRight _) =
        advance (moveRight dataTape) source
    
run dataTape source@(Tape _ MoveLeft _) =
        advance (moveLeft dataTape) source

run (Tape l p r) source@(Tape _ Increment  _) =
    advance (Tape l (p+1) r) source

run (Tape l p r) source@(Tape _ Decrement  _) =
    advance (Tape l (p-1) r) source

run dataTape@(Tape _ p _) source@(Tape _ Print _) = do
    putChar (chr p)
    hFlush stdout
    advance dataTape source

run dataTape@(Tape l _ r) source@(Tape _ Read _) = do
    p <- getChar
    advance(Tape l (ord p) r) source

run dataTape@(Tape _ p _) source@(Tape _ LoopL  _)
    | p == 0 = seekLoopR 0 dataTape source
    | otherwise = advance dataTape source

run dataTape@(Tape _ p _) source@(Tape _ LoopR  _)
    | p /= 0 = seekLoopL 0 dataTape source
    | otherwise = advance dataTape source

run dataTape source@(Tape _ (Comment _) _) = advance dataTape source
