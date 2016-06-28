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
  Token.reservedOpNames = ["let", "be", "in", "for", "as", "(x)", "*", "-o", 				   "promote", "discard", "derelict", "copy" ] }

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

-- Parsing table for types
