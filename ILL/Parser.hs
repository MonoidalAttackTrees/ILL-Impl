{-# LANGUAGE NoMonomorphismRestriction, PackageImports #-}
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
import Control.Monad -- For debugging messages.
import Data.Functor.Identity

import Syntax

lexer = haskellStyle {
  Token.reservedOpNames = ["let", "be", "in", "for", "as", "(x)", 
			   "unit", "-o", "\\"] }

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

unexpColon msg = unexpected msg
------------------------------------------------------------------------
-- Type Parsers							      --
------------------------------------------------------------------------
tyUnit = do
 reservedOp "unit"
 return I

tyLolly = do
 ty1 <- typeParser
 ws
 reservedOp "-o"
 ws
 ty2 <- typeParser
 return $ Lolly ty1 ty2

tyTensor = do
 ty1 <- typeParser
 ws
 reservedOp "(x)"
 ws
 ty2 <- typeParser
 return $ TensorTy ty1 ty2

------------------------------------------------------------------------
-- Parse tables							      --
------------------------------------------------------------------------
table = [[binOp AssocRight "(x)" (\d r -> TensorTy d r)],
         [binOp AssocLeft "-o" (\e s -> Lolly e s)]]
binOp assoc op f = Text.Parsec.Expr.Infix (do{ reservedOp op;ws;return f}) assoc
typeParser = buildExpressionParser table typeParser'
typeParser' = parens typeParser <|> tyUnit
------------------------------------------------------------------------
-- Term parsers							      --
------------------------------------------------------------------------
termParser = ws >> (parens termParser' <|> termParser')
termParser' = lamParse <|> appParse <|> unitParse <|> var

var = var' varName Var
var' p c = do 
    var_name <- p
    return (c var_name)  

varName = varName' isUpper "Term variables must begin with a lowercase letter."
varName' p msg = do
    n <- many alphaNum
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
    name <- varName
    dot
    body <- termParser
    colon
    ty <- typeParser
    return $ Lam ty . bind name $ body

appParse = do
    l <- many termParser
    case l of
        [] -> fail "No term to App"
        _  ->return $ foldl1 App l

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


