{-# LANGUAGE NoMonomorphismRestriction, PackageImports #-}
------------------------------------------------------------------------
-- This file implements the RecCalc parsers.  This file uses a       --
-- library called Parsec which implements parser combinators.  This   --
-- is an example of an application using higher-order functions; each --
-- parser is simply a higher-order function.                          -- 
--                                                                    --
-- Course: CSCI-3300 (Fall 2014)                                      --
-- Instructor: Prof. Eades                                            --
------------------------------------------------------------------------
module Parser where

import Prelude
import Data.List
import Data.Char 
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language
import Control.Monad -- For debugging messages.
import Data.Functor.Identity

import Syntax

------------------------------------------------------------------------
-- We first setup the lexer.                                          --
------------------------------------------------------------------------
lexer = haskellStyle {
  Token.reservedOpNames = ["fun", "with", "in", "=>", "Nat", "0", "rec", "||", 
                           "app", "to", "->", "suc" ]
}
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

unexpColon msg = unexpected msg -- Used for error handing.

------------------------------------------------------------------------
-- First, we implement the parser for types called typeParser.        --
-- We break this down into two main parsers tyNat, and binOp.         --
-- These are then composed into the final typeParser.                 --
------------------------------------------------------------------------
var' p c = do 
  var_name <- p
  return (c var_name)  

varName' p msg = do
  n <- many alphaNum
  ws
  when ((length n) > 0) $
    let h = head n in 
      when (p h || isNumber h) $ unexpColon (n++" : "++msg)
  return . s2n $ n

tyNat = do
  reservedOp "Nat"
  return Nat

-- The initial expression parsing table for types.
table = [[binOp AssocRight "->" (\d r -> Arr d r)]]
binOp assoc op f = Text.Parsec.Expr.Infix (do{ reservedOp op;ws;return f}) assoc
typeParser = buildExpressionParser table typeParser'
typeParser' = parens typeParser <|> tyNat

------------------------------------------------------------------------
-- termParser is the main parser for RecCalc programs.  It is         --
-- broken down into six parsers: funParse (function parser), recParse --
-- (parser for recursor), sucParse (parses the successor), zeroParse  --
-- (parses 0), and finally var (parse variables).                     --
------------------------------------------------------------------------
termParser = ws >> (parens termParser' <|> termParser')
termParser' = funParse <|> recParse <|> sucParse <|> appParse <|> zeroParse <|> var

varName = varName' isUpper "Term variables must begin with a lowercase letter."
var = var' varName Var

zeroParse = do
  reservedOp "0"
  return Zero

sucParse = do
  reservedOp "suc"
  ws
  t <- termParser
  return $ Suc t

funParse = do
  reservedOp "fun"
  ws
  name <- varName
  ws
  colon
  ws
  ty <- typeParser
  ws
  reservedOp "=>"
  ws
  body <- termParser
  return . Fun ty . bind name $ body

recParse = do
  reservedOp "rec"
  ws
  t <- termParser
  ws
  reservedOp "with"
  ws
  t1 <- termParser
  ws
  reservedOp "||"
  ws
  t2 <- termParser
  return $ Rec t t1 t2

appParse = do
  reservedOp "app"
  ws
  t1 <- termParser
  ws
  reservedOp "to"
  ws
  t2 <- termParser
  return . App t1 $ t2

------------------------------------------------------------------------
-- parseTerm and parseType are two functions that take in a string    --
-- and return either a term or a type using the respective parser.    --
-- If the string fails to parse, then an error is thrown.             --
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

------------------------------------------------------------------------
-- The final two parsers, parseTyCtx and parseTmCtx use the previous  --
-- parsers to parse in typing and term contexts.                      --
------------------------------------------------------------------------

tmPairCtxParse = do
  nm <- varName
  ws
  colon
  ws
  ty <- typeParser
  ws
  return (nm, ty)

tmCtxParse = tmPairCtxParse `sepBy` (Token.symbol tokenizer ",")
