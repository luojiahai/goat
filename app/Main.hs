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
import System.Environment
import System.Exit
import System.IO


data Task = 
  Compile | Parse | Pprint
  deriving (Show, Eq)

main :: IO ()
main = 
  do
    progname <- getProgName
    args <- getArgs
    task <- checkArgs progname args
    if task == Compile then
      do
        putStrLn "Sorry, cannot generate code yet"
        exitWith ExitSuccess
    else
      if task == Parse then
        do
          let [_, filename] = args
          input <- readFile filename
          let output = ast input
          case output of
            Right tree -> putStrLn (show tree)
            Left err -> do { putStr "Parse error at "
                           ; print err
                           ; exitWith (ExitFailure 2)
                           }
      else
        do
          let [_, filename] = args
          input <- readFile filename
          let output = ast input
          case output of
            Right tree -> prettyPrint tree
            Left err -> do { putStr "Parse error at "
                           ; print err
                           ; exitWith (ExitFailure 2)
                           }

-- processTask :: [String] -> Task -> IO ()
-- processTask 

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