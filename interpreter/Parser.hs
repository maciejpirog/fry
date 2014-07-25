module Parser where

import Control.Exception (throw)
import Text.Parsec
import qualified Text.Parsec.Token as Lex
import Text.Parsec.Language
import Text.Parsec.Expr
import qualified Data.Map as Map
import qualified Data.List as List

import Debug.Trace

import qualified AST as AST
import TheMonad

type Parser = Parsec String ()

keywords, resOps :: [String]
keywords = ["print", "let", "and", "be", "in", -- core
            "object", "extends", "fun", "end", "if", "then", "else", "true", "false"] -- sugar
resOps = ["->"] ++ map return "=.;@*/+-$?~"

fryStyle :: LanguageDef st
fryStyle
  = emptyDef
  { Lex.commentStart = "/%"
  , Lex.commentEnd = "%/"
  , Lex.commentLine = "%"
  , Lex.nestedComments = True
  , Lex.identStart = letter
  , Lex.identLetter = alphaNum <|> oneOf "_'"
  , Lex.reservedNames = keywords
  , Lex.reservedOpNames = resOps
  , Lex.caseSensitive = True
  }

lexer :: Lex.TokenParser ()
lexer = Lex.makeTokenParser fryStyle

whiteSpace = Lex.whiteSpace lexer
lexeme     = Lex.lexeme lexer
symbol     = Lex.symbol lexer
natural    = Lex.natural lexer
integer    = Lex.integer lexer
parens     = Lex.parens lexer
braces     = Lex.braces lexer
brackets   = Lex.brackets lexer
semi       = Lex.semi lexer
identifier = Lex.identifier lexer
reserved   = Lex.reserved lexer
reservedOp = Lex.reservedOp lexer
stringLit  = Lex.stringLiteral lexer

table = [ [op "$" mkapp AssocRight]
        , [op "*" astop AssocLeft, op "/" astop AssocLeft]
        , [op "+" astop AssocLeft, op "-" astop AssocLeft]
        , [op ">" astop AssocLeft, op "<" astop AssocLeft
          ,op ">=" astop AssocLeft, op "<=" astop AssocLeft]
        , [op "&&" astop AssocLeft, op "||" astop AssocLeft]
        , [op ";" astop AssocRight]
        ]
  where
    op s f assoc =
      Infix ((try (reservedOp s) >> return (f s)) <?> "operator") assoc
    astop = AST.Operator
    mkapp _ p q = makeLambdaApps [p, q]

--
-- Object declarations
--

smartLet :: [(AST.Name, AST.Object)] -> AST.Body -> AST.Program
smartLet [] (AST.Body p) = p
smartLet xs b            = AST.Let progs b
 where
  progs = map (\(n,o) -> (n, AST.Value (AST.New o))) xs

decls :: Parser AST.Program
decls = do cd <- many classDecl
           p <- expr
           --let cd' = map (\(n,o) -> (n, AST.Value (AST.New o))) cd
           return $ smartLet cd (AST.Body p)

classDecl :: Parser (AST.Name, AST.Object)
classDecl = do reserved "object"
               n <- identifier
               s <- option "" (reserved "extends" >> identifier)
               obj@(AST.Object m) <- object
               return (n, if s == ""
                            then obj
                            else AST.Object (Map.insert "super" (AST.Method "" (AST.Ident s)) m))
          <|> do reserved "fun"
                 (name, AST.Method _ (AST.Value (AST.New body)))
                   <- methodDecl undefined
                 reserved "end"
                 return (name, body)
          <?> "object definition"

--
-- Expressions
--

expr :: Parser AST.Program
expr =    buildExpressionParser table program
     <?> "expression"

program :: Parser AST.Program
program =   do reserved "let"
               cs <- letClause `sepBy1` reserved "and"
               reserved "in"
               p <- decls
               return (AST.Let cs (AST.Body p))
        <|> do reserved "if" -- if-then-else
               b <- decls
               reserved "then"
               p1 <- decls
               reserved "else"
               p2 <- decls
               return (AST.If b p1 p2)
        <|> do symbol "\\" -- lambda abstraction
               ns <- many1 lambdaIdent
               reservedOp "->"
               p <- decls
               return (makeLambdas ns p)
        <|> do apps <- many1 access
               return (makeLambdaApps apps)
        <?> "expression"

data LambdaIdent = LamCBN AST.Name | LamCBV AST.Name

lambdaIdent :: Parser LambdaIdent
lambdaIdent = do
  f <- option LamCBV (symbol "~" >> return LamCBN)
  x <- (identifier <|> (symbol "_" >> return ""))
  return (f x)

makeLambdaApps :: [AST.Program] -> AST.Program
makeLambdaApps [x] = x
makeLambdaApps (f:x:xs) = makeLambdaApps $
  (AST.Invoke (AST.Update f AST.CBV ["arg"] (AST.field x)) "val") : xs

makeLambdas :: [LambdaIdent] -> AST.Program -> AST.Program
makeLambdas []     p = p
makeLambdas (n:ns) p = makeLambda n (makeLambdas ns p)

makeLambda :: LambdaIdent -> AST.Program -> AST.Program
makeLambda n p =
  AST.Value $ AST.New $ AST.Object $
    Map.fromList [ ("arg", AST.Method "" AST.nil), ("val", val n)]
 where
   val (LamCBV n) = AST.Method n (AST.subst n (AST.Invoke (AST.Ident n) "arg") p)

letClause :: Parser (AST.Name, AST.Program)
letClause = do n <- identifier
               reserved "be"
               e <- decls
               return (n, e)

access :: Parser AST.Program
access = do p <- factor
            xs <- many invokeUpdate
            return (aux p xs)
 where
  aux p []             = p
  aux p (Left s : xs)  = aux (AST.Invoke p s) xs
  aux p (Right (cb, ns, m) : xs) = aux (AST.Update p cb ns m) xs

invokeUpdate :: Parser (Either AST.Name (AST.CB, [AST.Name], AST.Method))
invokeUpdate = 
     do symbol "."
        s <- identifier
        return (Left s)
 <|> do x <- braces update
        return (Right x)

factor :: Parser AST.Program
factor =   do i <- identifier
              return (AST.Ident i)
       <|> parens decls   
       <|> do ob <- object
              return (AST.Value $ AST.New ob)
       <|> do reservedOp "?"
              return (AST.Value $ AST.Error)
       <|> do n <- natural
              return (AST.Value $ AST.Int n)
       <|> do s <- stringLit
              return (AST.Value $ AST.Str s)
       <|> do reserved "true"
              return (AST.Value $ AST.Int 1)
       <|> do reserved "false"
              return (AST.Value $ AST.Int 0)
       <|> do try (reserved "print")
              x <- factor;
              return (AST.Print x)
       <?> "expression"

object :: Parser AST.Object
object = do methods <- brackets
                       (methodDecl "this" `sepBy` reservedOp ",")
            return (AST.Object (Map.fromList methods))
            <?> "object definition"

methodDecl :: AST.Name -> Parser (AST.Name, AST.Method)
methodDecl dt = do lhs <- identifier
                   args <- many lambdaIdent
                   rhs <- methodBody dt args
                   return (lhs, rhs)
              <?> "field definition"

methodBody :: AST.Name -> [LambdaIdent] -> Parser AST.Method
methodBody dt xs = do s <- this dt
                      reservedOp "="
                      rhs <- decls
                      return (AST.Method s (makeLambdas xs rhs))

updatePath :: Parser [AST.Name]
updatePath = identifier `sepBy` reservedOp "."

update :: Parser (AST.CB, [AST.Name], AST.Method)
update = do cb <- option AST.CBV (reservedOp "~" >> return AST.CBN)
            ns <- updatePath
            args <- many lambdaIdent
            m <- methodBody "" args
            return (cb, ns, m)

this :: AST.Name -> Parser AST.Name
this dt =   try (symbol "@" >> identifier)
        <|> try (symbol "@" >> symbol "_" >> return "")
        <|> return dt

expressionParser :: Parser AST.Program
expressionParser = do
  whiteSpace
  x <- decls
  eof
  return x

moduleParser :: Parser AST.Program
moduleParser = do
  whiteSpace
  cd <- many classDecl
  p <- option (AST.nil) expr
  eof
  return $ smartLet cd (AST.Body p)

runModuleParser :: String -> AST.Program
runModuleParser input =
  case (parse moduleParser "" input) of
    Left  err -> throw $ RuntimeException $ "parse error at " ++ show err
    Right x   -> x

parser :: String -> AST.Program
parser input = case (parse expressionParser "" input) of
  Left  err -> throw $ RuntimeException $ "parse error at " ++ show err
  Right x   -> x

