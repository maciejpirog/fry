module Ast2Hoas where

import Prelude hiding (lookup)
import Control.Exception
import Control.DeepSeq
import TheMonad
import qualified Data.List as List
import Data.Maybe
import qualified Data.Map as Map

import qualified AST as AST
import qualified Objects as HOAS

{-
type Table = Map.Map AST.Name HOAS.Program

(##) :: [(AST.Name, HOAS.Program)] -> Table -> Table
xs ## t = Map.union (Map.fromList xs) t

lookup :: Table -> AST.Name -> HOAS.Program
lookup st n = case Map.lookup n st of
  Just n  -> n
  Nothing -> throw $ RuntimeException $ "unknown identifier " ++ n

emptyTable = Map.empty
-}

------------------------------

type Table = [(AST.Name, HOAS.Program)]

(##) :: [(AST.Name, HOAS.Program)] -> Table -> Table
(##) = (++)

lookup :: Table -> AST.Name -> HOAS.Program
lookup st n = case List.lookup n st of
  Just n  -> n
  Nothing -> throw $ RuntimeException $ "unknown identifier " ++ n

emptyTable = []

----------------------------------

program :: Table -> AST.Program -> HOAS.Program
program st (AST.Invoke p n) = HOAS.Invoke (program st p) n
program st (AST.Update p c ns m) = HOAS.chainUpdate (program st p) (cb c) ns (method st m)
program st (AST.Let nps (AST.Body p)) = HOAS.Let ps (HOAS.Body p')
 where
  p'  = \v -> program (st'' v) p
  ps  = map (lookup st' . fst) nps
  st' = map (\(x,y) -> (x, program st' y)) nps ## st
  st'' v = zip (map fst nps) (map HOAS.Value v) ## st
program st (AST.Operator s p1 p2) =
  HOAS.Operator s (program st p1) (program st p2)
program st (AST.If b p1 p2) =
  HOAS.If (program st b) (program st p1) (program st p2)
program st (AST.Print p) = HOAS.Print (program st p)
program st (AST.Value v) = HOAS.Value (value st v)
program st (AST.Ident n) = lookup st n

value :: Table -> AST.Value -> HOAS.Value
value st (AST.New obj) = HOAS.New (object st obj)
value st (AST.Error)   = HOAS.Error
value st (AST.Int n)   = HOAS.Int n
value st (AST.Str s)   = HOAS.Str s

method :: Table -> AST.Method -> HOAS.Method
method st (AST.Method n p) =
 HOAS.Method (\obj@(HOAS.Object m) -> program (
    ( [(n, HOAS.Value (HOAS.New obj))]) ## st) p)

fromMethod :: HOAS.Method -> HOAS.Object -> HOAS.Program
fromMethod (HOAS.Method f) ob = f ob

object :: Table -> AST.Object -> HOAS.Object
object st (AST.Object m) = HOAS.Object $ fmap (method st) m

cb :: AST.CB -> HOAS.CB
cb AST.CBN = HOAS.CBN
cb AST.CBV = HOAS.CBV

identError :: AST.Name -> a
identError i = throw $ RuntimeException $ "unknown identifier \"" ++ i ++ "\""

-- "outer" is used on the outer level, when we load a list of
-- objects into the interactive interpreter. There is a special
-- case if the outer expression is a "let": in such a case the
-- defined values stay in the global namespace, so they can be
-- called from the interactive interpreter or from a file that
-- imports this file as a library.
outer :: Table -> AST.Program -> (HOAS.Program, Table)
outer st (AST.Let nps (AST.Body p)) =
 (HOAS.Let ps (HOAS.Body p'), st')
  where
   p'  = \v -> program (st'' v) p
   ps  = map (lookup st' . fst) nps
   st' = map (\(x,y) -> (x, program st' y)) nps ## st
   st'' v = zip (map fst nps) (map HOAS.Value v) ## st
outer st p = (program st p, st)
