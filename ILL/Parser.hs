{-# LANGUAGE NoMonomorphismRestriction, PackageImports,
             FlexibleContexts, PackageImports #-}
------------------------------------------------------------------------
-- This file contains the ILL-Impl parsers utilizing Parsec library.  --
------------------------------------------------------------------------
module Parser where

import Prelude
import Data.List
import Data.Char 
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language
import Control.Monad
import Data.Functor.Identity

import Syntax

lexer = haskellStyle {
  Token.reservedNames = ["let", "be", "in", "for", "as", "unit", "I"],
  Token.reservedOpNames = ["(x)", "tens", "-o", "\\"] }

tokenizer = Token.makeTokenParser lexer

ident      = Token.identifier tokenizer
reserved   = Token.reserved tokenizer
reservedOp = Token.reservedOp tokenizer
parens     = Token.parens tokenizer
angles     = Token.angles tokenizer
brackets   = Token.brackets tokenizer
braces     = Token.braces tokenizer
ws         = Token.whiteSpace tokenizer
natural    = Token.natural tokenizer
dot        = Token.dot tokenizer
comma      = Token.comma tokenizer
colon      = Token.colon tokenizer
symbol     = Token.symbol tokenizer
identifier = Token.identifier tokenizer

unexpColon msg = unexpected msg
------------------------------------------------------------------------
-- Type Parsers							      --
------------------------------------------------------------------------
tyUnit = do
 reservedOp "I"
 return I

------------------------------------------------------------------------
-- Parse tables							      --
------------------------------------------------------------------------
tyTable = [[binOp AssocLeft "(x)" (\d r -> Tensor d r)],
         [binOp AssocRight "-o" (\d r -> Lolly d r)]]
binOp assoc op f = Text.Parsec.Expr.Infix (do{ reservedOp op;return f}) assoc
typeParser = buildExpressionParser tyTable typeParser'
typeParser' = parens typeParser <|> tyUnit

------------------------------------------------------------------------
-- Term parsers							      --
------------------------------------------------------------------------
aterm = parens termParser <|> unitParse <|> var
termParser = lamParse <|> try letTParse <|> letUParse <|> tensParse <|> appParse <?> "Parser error"

var = var' varName Var
var' p c = do 
    var_name <- p
    return (c var_name)  

varName = varName' isUpper "Term variables must begin with a lowercase letter."
varName' p msg = do
    n <- identifier
    when ((length n) > 0) $
     let h = head n in 
       when (p h || isNumber h) $ unexpColon (n++" : "++msg)
    return . s2n $ n

unitParse = do
    reserved "unit"
    return Unit

lamParse = do
    reserved "\\"
    symbol "("               
    name <- varName
    colon
    ty <- typeParser
    symbol ")"
    dot
    body <- termParser    
    return $ Lam ty . bind name $ body

appParse = do
    l <- many1 aterm    
    return $ foldl1 App l

tensParse = do
   reserved "tens"
   t1 <- termParser
   symbol ","
   t2 <- termParser
   return $ Tens t1 t2
          
letUParse = do
    reserved "let"
    reserved "unit"
    reservedOp "="
    t1 <- termParser
    reserved "in"
    t2 <- termParser
    return $ LetU t1 t2

letTParse = do
    reserved "let"
    x <- varName
    reservedOp "(x)"
    y <- varName
    reservedOp "="
    t1 <- termParser
    reserved "in"
    t2 <- termParser
    return $ LetT t1 (bind x (bind y t2))
------------------------------------------------------------------------
-- Functions String -> Term or String -> Type			      --
------------------------------------------------------------------------      
parseTerm :: String -> Term
parseTerm str = 
    case parse termParser "" str of
        Left e  -> error $ show e
        Right r -> r

parseType :: String -> Type
parseType str = 
    case parse typeParser "" str of
        Left e  -> error $ show e
        Right r -> r

parseTester p str = 
    case parse p "" str of
        Left e -> error $ show e
        Right r -> r 

------------------------------------------------------------------------                 
-- Parsers for the REPL                                               --
------------------------------------------------------------------------
data REPLExpr =
   ShowAST Term
 | DumpState
 | Unfold Term
 deriving Show

replTermCmdParser short long c p = do
  colon
  cmd <- many lower
  ws
  t <- p
  eof
  if (cmd == long || cmd == short)
  then return $ c t
  else fail $ "Command \":"++cmd++"\" is unrecognized."

replIntCmdParser short long c = do
  colon
  cmd <- many lower
  ws
  eof
  if (cmd == long || cmd == short)
  then return c
  else fail $ "Command \":"++cmd++"\" is unrecognized."

showASTParser = replTermCmdParser "s" "show" ShowAST termParser

unfoldTermParser = replTermCmdParser "u" "unfold" Unfold termParser         

dumpStateParser = replIntCmdParser "d" "dump" DumpState

lineParser = try showASTParser <|> try unfoldTermParser <|> try dumpStateParser

parseLine :: String -> Either String REPLExpr
parseLine s = case (parse lineParser "" s) of
                Left msg -> Left $ show msg
                Right l -> Right l
        
