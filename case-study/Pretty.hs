{-# LANGUAGE FlexibleContexts #-}
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
prettyType Nat = return "Nat"
prettyType (Arr x y) = do
	xty <- prettyType x
	yty <- prettyType y
	return $ xty ++ " -> " ++ yty

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
prettyTerm Zero = return "0"
prettyTerm (Suc t) = do
	s <- prettyTerm t
	return $ "suc " ++ s ++ ""
prettyTerm (Var name) = do
	let var = n2s name
	return var
prettyTerm (App t1 t2) = do
	tm1 <- prettyTerm t1
	tm2 <- prettyTerm t2
	return $ "app " ++ tm1 ++ " to " ++ tm2 ++ " "
prettyTerm (Fun ty tm) = do
	(n,tm') <- unbind tm 
	str <- prettyTerm tm'
	tystr <- prettyType ty
	return $ "fun " ++ (n2s n) ++ " : " ++ tystr ++ " => (" ++ str ++ ")"
prettyTerm (Rec t1 t2 t3) = do
	tm1 <- prettyTerm t1
	tm2 <- prettyTerm t2
	tm3 <- prettyTerm t3
	return $ "rec " ++ tm1 ++ "with " ++ tm2 ++ " || " ++ tm3 ++ " "

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
