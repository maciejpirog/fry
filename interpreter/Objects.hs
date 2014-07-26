module Objects where

import Data.List
import Data.Map as Map
import Data.Maybe
import Control.Exception
import Control.Monad
import Control.Monad.Writer

import Debug.Trace

import TheMonad

-- SYNTAX

type Name = String

data CB = CBV | CBN
  deriving(Show)

data Program = Invoke Program Name
             | Update Program CB Name Method
             | Let [Program] Body
             | Operator String Program Program
             | If Program Program Program
             | Print Program
             | Value Value
             deriving(Show)

newtype Body = Body ([Value] -> Program)

data Value = New Object
           | Error
           | Int Integer
           | Str String

newtype Method = Method (Object -> Program)

newtype Object = Object (Map.Map Name Method)

instance Show Object where
  show (Object x) = show $ fmap fst (Map.toList x)

instance Show Method where show _ = "(...)"
instance Show Body where show _ = "(...)"

instance Show Value where
  show (New obj) = show obj
  show (Int i)   = show i
  show (Str s)   = s

-- HELPERS

unit :: Value
unit = New (Object Map.empty)

readField :: Method -> Object -> Program
readField (Method m) obj = m obj

getField :: Name -> Object -> Program
getField n (Object m) = readField ((Map.!) m n) (throw $ RuntimeException "not a field!") 

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
        Nothing -> throw $ RuntimeException $ "no method \"" ++ n ++ "\" in " ++ show obj

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
      Nothing -> throw $ RuntimeException $ "no method \"" ++ n ++ "\" in " ++ show obj

reduce :: Program -> TheMonad Value
reduce p = do
  --statRed
  reduce' p

reduce' :: Program -> TheMonad Value
reduce' (Operator s p1 p2) = do
  v1 <- reduce p1
  v2 <- reduce p2
  operator s v1 v2
reduce' (If b p1 p2) = do
  b' <- reduce b
  reduceIf b' p1 p2
reduce' (Invoke p l) = do
  o' <- reduce p
  case o' of
    New o -> do m <- getMethod o l
                reduceMethod o m
    _ -> throw $ RuntimeException $ (show o') ++ " is not an object"
reduce' (Update p CBV l (Method f)) = do
  v <- reduce p
  case v of
   New o -> do k <- reduce (f o)
               o' <- updateMethod o l (programToMethod (Value k))
               return (New o')
   k -> throw $ RuntimeException $ "type error: " ++ show k ++ " not an object"
reduce' (Update p CBN l m) = do
  v <- reduce p
  case v of
   New o -> do o' <- updateMethod o l m
               return (New o')
   k -> throw $ RuntimeException $ "type error: " ++ show k ++ " not an object"
reduce' (Let ps (Body f)) = do
  vs <- mapM reduce ps
  reduce (f vs)
reduce' (Print p) = do
  x <- reduce p
  io $ putStr $ show x
  return unit
reduce' (Value Error) = throw $ RuntimeException $  "reached \"?\""
reduce' (Value v) = return v

reduceMethod :: Object -> Method-> TheMonad Value
reduceMethod o (Method p) =
  reduce (p o)

reduceIf :: Value -> Program -> Program -> TheMonad Value
reduceIf (Int 1) p _ = reduce p
reduceIf (Int 0) _ p = reduce p
reduceIf _ _ _       = throw $ RuntimeException $  "type error: expected a boolean" 

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

operator :: String -> Value -> Value -> TheMonad Value
operator ";" p1 p2 = do
  return p2
operator s p1 p2 |  (s `elem` fmap return "+-*/><")
                 || (s `elem` [">=", "<=", "||", "&&"]) =
  arith s p1 p2
operator s _ _ = throw $ RuntimeException $  "Unknown operator \"" ++ s ++ "\""

arith :: String -> Value -> Value -> TheMonad Value
arith s v1 v2 = do
  return $ Int (types v1 v2)
 where
  types (Int n1) (Int n2) = eval n1 n2
  types y z = throw $ RuntimeException $ "type error: expected integers in arith operator \"" ++ s ++ "\"\ngot " ++ show y ++ " and " ++ show z
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
