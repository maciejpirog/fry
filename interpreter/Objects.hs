module Objects where

import Data.List
import Data.Map as Map
import Data.Maybe
import Control.Monad
import Control.Monad.Writer

import Debug.Trace

import TheMonad

-- SYNTAX

type Name = String

data CB = CBV | CBN
  deriving(Show,Eq)

data Program = Invoke Program Name
             | Update Program CB Name Method
             | Let [Program] Body
             | Operator String Program Program
             | If Program Program Program
             | Print Program
             | Value Value
             deriving(Show)

data Body = Body ([Value] -> Program)

data Value = New Object
           | Error
           | Int Integer
           | Str String

data Method = Method (Object -> Program)

data Object = Object (Map.Map Name Method)

instance Show Method where show _ = "{method}"
instance Show Body where show _ = "{body}"

instance Show Value where
  show (New obj) = show obj
  show (Int i)   = show i
  show (Str s)   = s

--instance Show Method where show (Method f) = show (f undefined)
--instance Show Body where show (Body f) = "let " ++ show (f undefined)

instance Show Object where show (Object m) = show (toList m)

-- HELPERS

unit :: Value
unit = New (Object Map.empty)

readField :: Method -> Object -> Program
readField (Method m) obj = m obj

getField :: Name -> Object -> Program
getField n (Object m) = readField ((Map.!) m n) (error "not a field!") 

objectToMethod :: Object -> Method
objectToMethod o = Method $ \_ -> Value (New o)

programToMethod :: Program -> Method
programToMethod p = Method $ \_ -> p

-- EVALUATOR

getMethod :: Object -> Name -> TheMonad Method
getMethod obj@(Object o) n =
  case Map.lookup n o of
    Just m  -> return m
    Nothing ->
      case Map.lookup "super" o of
        Just s  -> do New so <- reduce (readField s obj)
                      getMethod so n
        Nothing -> error $ "no method \"" ++ n ++ "\""

updateMethod :: Object -> Name -> Method -> TheMonad Object
updateMethod obj@(Object o) n m =
  case Map.lookup n o of
    -- the method is declared on top-level -- overwrite here!
    Just _ -> return $ Object (Map.insert n m o)
    -- the method may be super's method
    Nothing -> case Map.lookup "super" o of
      Just s ->
        do New so <- reduce (readField s obj)
           so' <- updateMethod so n m
           let sm = objectToMethod so'
           return $ Object (Map.insert "super" sm o)
      Nothing -> error $ "no method \"" ++ n ++ "\""

reduce :: Program -> TheMonad Value
reduce p = do
  statRed
  (reduce' p)

reduce' :: Program -> TheMonad Value
reduce' (Operator s p1 p2) =
  operator s p1 p2
reduce' (If b p1 p2) = do
  b' <- reduce b
  reduceIf b' p1 p2
reduce' (Invoke p l) = do
  o' <- reduce p
  case o' of
    New o -> do m <- getMethod o l
                reduceMethod o m
    _ -> error (show o')
reduce' (Update p CBV l (Method f)) = do
  v <- reduce p
  case v of
   New o -> do k <- reduce (f o)
               o' <- updateMethod o l (programToMethod (Value k))
               return (New o')
   k -> error $ "trying to update a field " ++ l ++ " in " ++ show k
reduce' (Update p CBN l m) = do
  v <- reduce p
  case v of
   New o -> do o' <- updateMethod o l m
               return (New o')
   k -> error $ "trying to update a field " ++ l ++ " in " ++ show k
reduce' (Let ps (Body f)) = do
  vs <- mapM reduce ps
  reduce (f vs)
reduce' (Print p) = do
  x <- reduce p
  io $ putStr $ show x
  return unit
reduce' (Value Error) = error "undefined \"?\""
reduce' (Value v) = return v

reduceMethod :: Object -> Method-> TheMonad Value
reduceMethod o (Method p) =
  reduce (p o)

reduceIf :: Value -> Program -> Program -> TheMonad Value
reduceIf (Int 1) p _ = reduce p
reduceIf (Int 0) _ p = reduce p
reduceIf _ _ _       = error "type error: expected a boolean in if-then-else" 

-- SUGAR

extends :: Object -> Object -> Object
extends (Object o) p =
  Object $ Map.insert "super" (objectToMethod p) o

{- when your syntax is like
  p{a.b.c.d <- xyz}
  =
  p{a <- p.a{b <- p.a.b{c <- p.a.b.c{d <- xyz}}}}
do not confuse with
  p.b(c <- xyz)
-}
chainUpdate :: Program -> CB -> [Name] -> Method -> Program
chainUpdate q cb ns m = aux q ns
 where
  aux p [n] = Update p cb n m
  aux p (n:ns) = Let [p] $ Body $ \[v] -> Update (Value v) CBV n $
    programToMethod (chainUpdate (Invoke (Value v) n) cb ns m)

-- OPERATORS

operator :: String -> Program -> Program -> TheMonad Value
operator ";" p1 p2 = do
  reduce p1
  reduce p2
operator s p1 p2 |  (s `elem` fmap return "+-*/><")
                 || (s `elem` [">=", "<=", "||", "&&"]) =
  arith s p1 p2
operator s _ _ = error $ "Unknown operator \"" ++ s ++ "\""

arith :: String -> Program -> Program -> TheMonad Value
arith s p1 p2 = do
  v1 <- reduce p1
  v2 <- reduce p2
  return $ Int (types v1 v2)
 where
  types (Int n1) (Int n2) = eval n1 n2
  types y z = error $ "type error: expected integers in arith operator \"" ++ s ++ "\". Got " ++ show y ++ " and " ++ show z
  eval n1 n2 = op s n1 n2
  op "+" = (+)
  op "-" = (-)
  op "*" = (*)
  op "/" = div
  op "<" = ops (<)
  op ">" = ops (>)
  op ">=" = ops (>=)
  op "<=" = ops (<=)
  op "&&" = (*)
  op "||" = \x y -> bint (x == 1 || y == 1)
  bint True  = 1
  bint False = 0
  ops f a b = bint (f a b)
