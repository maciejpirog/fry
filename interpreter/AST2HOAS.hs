module AST2HOAS where

import Data.List
import Data.Maybe
import qualified Data.Map as Map

import qualified AST as AST
import qualified Objects as HOAS

type Table = AST.Name -> HOAS.Program 

(##) :: [(AST.Name, HOAS.Program)] -> Table -> Table
(xs ## t) n = t n `fromMaybe` lookup n xs

program :: Table -> AST.Program -> HOAS.Program
program st (AST.Invoke p n) = HOAS.Invoke (program st p) n
program st (AST.Update p c ns m) = HOAS.chainUpdate (program st p) (cb c) ns (method st m)
program st (AST.Let nps (AST.Body p)) = HOAS.Let ps (HOAS.Body p')
 where
  p'  = \v -> program (st'' v) p
  ps  = map (st' . fst) nps
  st' = map (\(x,y) -> (x, program st' y)) nps ## st
  st'' v = zip (map fst nps) (map HOAS.Value v) ## st
program st (AST.Operator s p1 p2) =
  HOAS.Operator s (program st p1) (program st p2)
program st (AST.If b p1 p2) =
  HOAS.If (program st b) (program st p1) (program st p2)
program st (AST.Print p) = HOAS.Print (program st p)
program st (AST.Value v) = HOAS.Value (value st v)
program st (AST.Ident n) = st n

value :: Table -> AST.Value -> HOAS.Value
value st (AST.New obj) = HOAS.New (object st obj)
value st (AST.Error)   = HOAS.Error
value st (AST.Int n)   = HOAS.Int n
value st (AST.Str s)   = HOAS.Str s

method :: Table -> AST.Method -> HOAS.Method
method st (AST.Method n p) =
  HOAS.Method (\obj@(HOAS.Object m) -> program (
    ( [(n, HOAS.Value (HOAS.New obj))]
      ++ fromMaybe [] (fmap (\m -> [("super", fromMethod m obj)]) (Map.lookup "super" m))) ## st) p)

fromMethod :: HOAS.Method -> HOAS.Object -> HOAS.Program
fromMethod (HOAS.Method f) ob = f ob

object :: Table -> AST.Object -> HOAS.Object
object st (AST.Object m) = HOAS.Object $ fmap (method st) m

cb :: AST.CB -> HOAS.CB
cb AST.CBN = HOAS.CBN
cb AST.CBV = HOAS.CBV

identError :: AST.Name -> a
identError i = error $ "unknown identifier \"" ++ i ++ "\""

ast2hoas :: AST.Program -> HOAS.Program
ast2hoas = program identError
