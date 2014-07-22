module Main where

import System.Environment
import Control.Monad
import Control.Monad.Writer

import qualified Objects as Ob
import qualified AST as AST
import qualified Parser as Parser
import AST2HOAS (ast2hoas)
import TheMonad

run :: String -> TheMonad Ob.Value
run s = Ob.reduce $ ast2hoas $ Parser.parser s

runFile :: Bool -> String -> IO ()
runFile verbose prog = do
  (val, Stats n) <- runWriterT $ run prog
  --putStrLn ""
  print val
  when verbose (putStrLn $ "(in " ++ show n ++ " reduction"
             ++ (if n /= 1 then "s" else "") ++ ")")

interp :: String -> IO ()
interp s = do
  f <- readFile s
  runFile True f

parseArgs :: [String] -> ([String], Bool)
parseArgs [] = ([], False)
parseArgs ("-v" : xs) = let (s, _) = parseArgs xs in (s, True)
parseArgs (x:xs) = let (s, b) = parseArgs xs in (x:s, b)

help :: IO ()
help = do
  putStrLn "Fry Interpterer ver. 0.0.1"
  putStrLn "No input files!"
  putStrLn "Usage: fry file1.fry file2.fry ... filek.fry"
  putStrLn "try also -v option for verbose statistics"

main :: IO ()
main = do
  (args, verbose) <- fmap parseArgs getArgs
  if null args
    then help
    else do
      files <- sequence (map readFile args)
      let program = concat (map (++"\n") files)
      runFile verbose program
      --putStrLn (concat files)
