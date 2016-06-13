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
eval Zero = return Zero
eval (Suc t) = eval t
eval (Fun ty tm) = return $ Fun ty tm
eval (App t1 t2) = do
 case t1 of 
  (Fun ty tm) -> do 
		   (bv,by) <- unbind tm
		   return $ replace bv t2 by	  
  _ -> return $ App t1 t2 -- not a beta redux
eval (Rec t0 t1 t2) = do
 case t0 of
  Zero    -> return t1
  (Suc t) -> return $ App (App t2 (Rec t0 t1 t2)) t0

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
