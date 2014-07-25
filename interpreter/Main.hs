module Main where

import Prelude hiding (interact)
import System.Environment
import Control.Exception
import Control.Monad
import Control.Monad.Writer
import System.IO hiding (interact)
import System.IO.Error

import qualified Objects as Ob
import qualified AST as AST
import qualified Parser as Parser
import qualified Ast2Hoas as A2H
import TheMonad

import Debug.Trace

run :: String -> TheMonad Ob.Value
run = Ob.reduce . A2H.ast2hoas . Parser.parser

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

parseFile :: String -> IO ()
parseFile s = do
  f <- readFile s
  let p = Parser.parser f
  putStrLn (show p)

-- INTERACTIVE

data Input = Prog String | Load String | Exit | Help

getInput :: IO Input
getInput = do
  putStr "Fry> "
  hFlush stdout
  d <- getData
  return $ interpData d
 where
  getData = do
    s <- getLine
    if not (null s) && last s == '\\'
      then getData >>= return . (init s ++ )
      else return s
  interpData (':':'l':' ':xs) = Load xs
  interpData (':':'q':_)      = Exit
  interpData (':':'?':_)      = Help
  interpData s                = Prog s

interact :: IO ()
interact = interact' A2H.identError

interact' :: A2H.Table -> IO ()
interact' t = do
  i <- getInput
  case i of
    Prog s -> crunch t s >>= interact'
    Load f -> getFile f >>= crunch t >>= interact'
    Help   -> help >> interact' t
    Exit   -> return ()

getFile :: String -> IO String
getFile s = catchIOError (readFile s')
  $ \err -> if isDoesNotExistError err
              then return $ "\"error: file " ++ s' ++" not found!\""
              else error "IO error!"
 where
  s' = if (reverse $ take 4 $ reverse s) /= ".fry"
         then s ++ ".fry"
         else s

crunch :: A2H.Table -> String -> IO A2H.Table
crunch t s = handle
 (\(RuntimeException s) -> putStrLn ("error: " ++ s) >> return t)
 $ do
    let ast = Parser.runModuleParser s
    let (hoas, t') = A2H.outer t ast
    v <- toIO $ Ob.reduce hoas
    putStrLn (show v)
    return t'

welcome :: IO ()
welcome = do
  putStrLn "Welcome to FRY Interpterer ver. 0.0.0"
  putStrLn "type :? for help\n"
  hFlush stdout

help :: IO ()
help = do
  putStrLn "FRY Interpterer ver. 0.0.0\n"
  putStrLn "Type in an expression and it will be evaluated. Top-level definitions (\"object\" and \"fun\" keywords) will be remembered until the end of the session. Alternatively, type one of the following commands:\n"
  putStrLn "  :?         display this message"
  putStrLn "  :l <file>  load the <file> program (no \".fry\" extension necessary)"
  putStrLn "  :q         exit the interpreter"
  hFlush stdout

main :: IO ()
main = do
  welcome
  interact
