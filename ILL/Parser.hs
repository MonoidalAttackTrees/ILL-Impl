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
  Token.reservedOpNames = ["let", "be", "in", "for", "as", "(x)", 
			   "unit", "-o", "\\", "I"] }

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
table = [[binOp AssocLeft "(x)" (\d r -> Tensor d r)],
         [binOp AssocRight "-o" (\d r -> Lolly d r)]]
binOp assoc op f = Text.Parsec.Expr.Infix (do{ reservedOp op;ws;return f}) assoc
typeParser = buildExpressionParser table typeParser'
typeParser' = parens typeParser <|> tyUnit

------------------------------------------------------------------------
-- Term parsers							      --
------------------------------------------------------------------------
aterm = parens termParser <|> unitParse <|> var 
termParser = lamParse <|> letUParse <|> letTParse <|> appParse <?> "Parser error"

var = var' varName Var
var' p c = do 
    var_name <- p
    return (c var_name)  

varName = varName' isUpper "Term variables must begin with a lowercase letter."
varName' p msg = do
    n <- many1 alphaNum
    ws
    when ((length n) > 0) $
     let h = head n in 
       when (p h || isNumber h) $ unexpColon (n++" : "++msg)
    return . s2n $ n

unitParse = do
    reservedOp "unit"
    return Unit

lamParse = do
    reservedOp "\\"
    symbol "("               
    name <- varName
    colon
    ty <- typeParser
    symbol ")"
    dot
    body <- termParser    
    return $ Lam ty . bind name $ body

appParse = do
    l <- many aterm
    case l of
        [] -> fail "No term to App"
        _  -> return $ foldl1 App l

letUParse = do
    reservedOp "let"
    t1 <- termParser
    reservedOp "="
    _ <- unitParse
    reservedOp "in"
    t2 <- termParser
    return $ LetU t1 t2

letTParse = do
    reservedOp "let"
    t1 <- termParser
    reservedOp "="
    x <- termParser
    reservedOp "(x)"
    y <- termParser
    ws
    reservedOp "in"   
    t2 <- termParser
    return $ LetT t1 (bind (s2n "x") (bind (s2n "y") t2))

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
