module Main where

import Control.Monad.Writer

import qualified Objects as Ob
import qualified AST as AST
import qualified Parser as Parser
import qualified AST2HOAS as A2H

--run :: String -> (Ob.Value, String)
run s = let ast  = Parser.parser s
            hoas = A2H.program identError ast
            writ = Ob.reduce hoas
         in snd $ runWriter writ

identError :: AST.Name -> a
identError i = error $ "unknown identifier \"" ++ i ++ "\""

--runFile :: String -> IO (Ob.Value, String)
runFile s = do prog <- readFile s
               putStr (run prog)

parseFile s = do prog <- readFile s
                 return $ Parser.parser prog
