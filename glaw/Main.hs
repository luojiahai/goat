---------------------------------------------------------------------
-- COMP90045 Programming Language Implementation                   --
-- Programming Project: Goat                                       --
--                                                                 --
-- Team: GOAT SIMULATOR                                            --
-- Members:                                                        --
--          Chenqin Zhang, Geoffrey Ka-Hoi Law, Yun Chen           --
--          733301, 759218, 760419                                 --
--          {chenqinz, glaw, yunc4}@student.unimelb.edu.au         --
---------------------------------------------------------------------

module Main where 

import GoatParser
import GoatPrettyPrinter
import GoatAnalyze
import System.Environment
import System.Exit


data Task = 
  Compile | Parse | Pprint
  deriving (Show, Eq)

-- Main function
main :: IO ()
main = 
  do
    progname <- getProgName
    args <- getArgs
    task <- checkArgs progname args
    processTask args task

-- Processes a task
processTask :: [String] -> Task -> IO ()
processTask args Compile =
  do
    putStrLn "Sorry, cannot generate code yet"
    let [filename] = args
    input <- readFile filename
    let output = ast input
    case output of
      Right tree -> putStrLn (show (analyze tree))
      Left err -> do 
                    putStr "Parse error at "
                    print err
                    exitWith (ExitFailure 2)
    exitWith ExitSuccess
processTask args Parse =
  do
    let [_, filename] = args
    input <- readFile filename
    let output = ast input
    case output of
      Right tree -> putStrLn (show tree)
      Left err -> do 
                    putStr "Parse error at "
                    print err
                    exitWith (ExitFailure 2)
processTask args Pprint =
  do
    let [_, filename] = args
    input <- readFile filename
    let output = ast input
    case output of
      Right tree -> prettyPrint tree
      Left err -> do 
                    putStr "Parse error at "
                    print err
                    exitWith (ExitFailure 2)

-- Checks command line arguments
checkArgs :: String -> [String] -> IO Task
checkArgs _ ['-':_] = 
  do
    putStrLn ("Missing filename")
    exitWith (ExitFailure 1)
checkArgs _ [filename] = 
  return Compile
checkArgs _ ["-p", filename] = 
  return Pprint
checkArgs _ ["-a", filename] = 
  return Parse
checkArgs progname _ = 
  do
    putStrLn ("Usage: " ++ progname ++ " [-p] filename")
    exitWith (ExitFailure 1)