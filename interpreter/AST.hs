module AST where

import Data.List
import Data.Map as Map

-- SYNTAX

type Name = String

data CB = CBV | CBN
  deriving(Show,Eq)

data Program = Invoke !Program !Name
             | Update !Program !CB [Name] !Method
             | Let [(Name, Program)] !Body
             | Operator !String !Program !Program
             | If !Program !Program !Program
             | Print !Program
             | Value !Value
             | Ident !Name
             deriving(Show)

data Body = Body Program deriving(Show)

data Value = New !Object
           | Error
           | Str !String
           | Int !Integer

data Method = Method Name !Program -- Name is the ident for "self"
  deriving(Show)

data Object = Object (Map.Map Name Method)

-- instance Show Method where show _ = "{method}"
-- instance Show Body where show _ = "{body}"

instance Show Value where
  show (New o) = show o
  show (Int i) = show i
  show (Str s) = s
  show (Error) = "?"

--instance Show Method where show (Method n f) = show n ++ show f
--instance Show Body where show (Body f) = "let " ++ show (f undefined)
instance Show Object where show (Object m) = "[" ++ Data.List.foldr (\x y -> x ++ ", " ++ y) "]" (fmap (\(n, Method this e) -> n ++ " " ++ "@" ++ this ++ " = " ++ show e) (toList m))

-- HELPERS

nil :: Program
nil = Value (New (Object (fromList [])))

field :: Program -> Method
field p = Method "" p

msg :: String -> Program
msg = Value . Str

-- SUBSTITUTING

subst :: Name -> Program -> Program -> Program
subst n p (Invoke t l) = Invoke (subst n p t) l
subst n p (Update t cb ns met)
  = Update (subst n p t) cb ns (substMethod n p met)
subst n p l@(Let xs (Body b))
  | n `elem` fmap fst xs = l
  | otherwise            = Let (fmap (\(x, q) -> (x, subst n p q)) xs) (Body $ subst n p b)
subst n p (Operator s t1 t2)
  = Operator s (subst n p t1) (subst n p t2)
subst n p (If b p1 p2)
  = If (subst n p b) (subst n p p1) (subst n p p2)
subst n p (Print t) = Print (subst n p t)
subst n p (Value v) = Value (substValue n p v)
subst n p i@(Ident k) | k == n    = p
                      | otherwise = i

substMethod :: Name -> Program -> Method -> Method
substMethod n p met@(Method m t)
  | n == m    = met
  | otherwise = Method m (subst n p t)

substValue :: Name -> Program -> Value -> Value
substValue n p val@(New (Object m))
  -- | n `Map.member` m = val
  | otherwise        = New $ Object (fmap (substMethod n p) m)
substValue n p i = i

