------------------------------------------------------------------------
-- This file implements the evaluation algorithm for RecCalc          --
-- programs.                                                          --
--                                                                    --
-- Course: CSCI-3300 (Fall 2014)                                      --
-- Instructor: Prof. Eades                                            --
------------------------------------------------------------------------
module Eval where

import Syntax
import TypeCheck
import Parser 
import Pretty

------------------------------------------------------------------------
-- The evaluator.                                                     --
------------------------------------------------------------------------
eval :: Fresh m => Term -> m Term
eval Zero = undefined
eval (Suc t) = undefined
eval (Fun ty tm) = undefined
eval (App t1 t2) = undefined
eval (Rec t0 t1 t2) = undefined
------------------------------------------------------------------------
-- This function make it easy to run the evaluator.                   --
------------------------------------------------------------------------
runEval :: Term -> Term
runEval = runFreshM.eval

------------------------------------------------------------------------
-- This function takes in a term as a string, parses it, evaluates    --
-- it, and then pretty prints its value.                              --
------------------------------------------------------------------------
testEval :: String -> String
testEval = runPrettyTerm.runEval.parseTerm

------------------------------------------------------------------------
-- This function tests a RecCalc program for type preservation.      --
------------------------------------------------------------------------
typePres :: Term -> Bool
typePres = undefined
