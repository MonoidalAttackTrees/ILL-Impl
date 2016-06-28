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
  Token.reservedOpNames = ["let", "be", "in", "for", "as", "(x)", "*", "-o", 				   "promote", "discard", "derelict", "copy", "unit" ] }

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
 return UnitTy

tyLolly = undefined

tyTensor = undefined

------------------------------------------------------------------------
-- Parse tables							      --
------------------------------------------------------------------------
table = [[binOp AssocRight "(x)" (\d r -> TensorTy d r)]]
         --[binOp AssocRight "-o" (\e s -> Lolly e s]]
binOp assoc op f = Text.Parsec.Expr.Infix (do{ reservedOp op;ws;return f}) assoc
typeParser = buildExpressionParser table typeParser'
typeParser' = parens typeParser <|> tyUnit

------------------------------------------------------------------------
-- Term parsers							      --
------------------------------------------------------------------------
termParser = undefined



