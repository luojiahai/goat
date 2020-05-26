module Main (main) 
where

-------------------------------------------------------------------------
--  Main function for a Goat compiler.  
--  At this stage, only a parser and pretty-printer has been implemented.
--
--  Harald Sondergaard, April 2019
-------------------------------------------------------------------------

import GoatParser (ast)
import Analyze
import CodeGen
import PrettyPrinter (prettyPrint)
import System.Environment (getProgName, getArgs)
import System.Exit (exitWith, ExitCode(..))

data Task
  = Pprint | Compile | Parse
    deriving (Eq, Show)

main :: IO ()
main
  = do
      progname <- getProgName
      args <- getArgs
      task <- checkArgs progname args
      case task of
        Compile 
          -> do
               let [filename] = args
               input <- readFile filename
               let output = ast input
               case output of
                 Right tree -> do
                                 let tables = analyze tree
                                 let code = codegen tree tables
                                 putStrLn (code)
                 Left err -> do 
                               putStr "Parse error at "
                               print err
                               exitWith (ExitFailure 2)
               exitWith ExitSuccess
        Parse
          -> do
               let [_, filename] = args
               input <- readFile filename
               let output = ast input
               case output of
                 Right tree 
                   -> putStrLn (show tree)
                 Left err 
                   -> do { putStr "Parse error at "
                         ; print err
                         ; exitWith (ExitFailure 2) 
                         }
        Pprint
          -> do
               let [_, filename] = args
               input <- readFile filename
               let output = ast input
               case output of
                 Right tree 
                   -> putStrLn (prettyPrint tree)
                 Left err 
                   -> do { putStr "Parse error at "
                         ; print err
                         ; exitWith (ExitFailure 2) 
                         }

checkArgs :: String -> [String] -> IO Task
checkArgs _ ['-':_]
  = do
      putStrLn ("Missing filename")
      exitWith (ExitFailure 1)
checkArgs _ [filename]
  = return Compile
checkArgs _ ["-p", filename]
  = return Pprint
checkArgs _ ["-a", filename]
  = return Parse
checkArgs progname _
  = do
      putStrLn ("Usage: " ++ progname ++ " [-p] filename")
      exitWith (ExitFailure 1)
