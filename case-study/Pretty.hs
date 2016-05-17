------------------------------------------------------------------------
-- This file implements the RecCalc pretty printers.                 --
--                                                                    --
-- Parentheses are added to the output where necessary.  See the      --
-- handout for more details.                                          --
--                                                                    --
-- Course: CSCI-3300 (Fall 2014)                                      --
-- Instructor: Prof. Eades                                            --
------------------------------------------------------------------------
module Pretty where

import Data.List
import Data.Char
import Text.Parsec

import Syntax
import Parser 

------------------------------------------------------------------------
-- prettyType converts a type into a string.                          --
------------------------------------------------------------------------
prettyType :: Fresh m => Type -> m String
prettyType = undefined

------------------------------------------------------------------------
-- Some helpful testing functions.                                    --
------------------------------------------------------------------------
testPretty parser pretty s = do
  let o = parse parser "" s in  
    case o of
      Left e -> error $ show e
      Right r -> runFreshM (pretty r)

testPrettyType :: String -> String
testPrettyType = testPretty typeParser prettyType

------------------------------------------------------------------------
-- prettyTerm converts a term into a string.                          --
------------------------------------------------------------------------
prettyTerm :: Fresh m => Term -> m String
prettyTerm = undefined

------------------------------------------------------------------------
-- A helpful testing function, and a couple of funcations that make   --
-- it easy to run the pretty printers.                                --
------------------------------------------------------------------------
testPrettyTerm :: String -> String
testPrettyTerm = testPretty termParser prettyTerm

runPrettyType :: Type -> String
runPrettyType = runFreshM.prettyType

runPrettyTerm :: Term -> String
runPrettyTerm = runFreshM.prettyTerm