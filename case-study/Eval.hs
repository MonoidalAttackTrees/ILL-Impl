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
eval (Suc t) = do 
 t' <- eval t
 return (Suc t')
eval (Fun tyA tm) = do
 (x,bdy) <- unbind tm
 bdy' <- eval bdy
 return $ Fun tyA $ bind x bdy'  
eval (App t1 t2) = do
 case t1 of 
  (Fun ty tm) -> do 
		   (bv,by) <- unbind tm
		   return $ replace bv t2 by	  
  _ -> return $ App t1 t2 -- not a beta redux
eval (Rec t0 t1 t2) = do
 t0' <- eval t0
 t1' <- eval t1
 t2' <- eval t2 
 case t0' of
  Zero    -> return t1'
  (Suc t) -> do
	term <- eval $ App (App t2' (Rec t0' t1' t2')) t0' 
	return term
  _       -> error "Evaluator Error: Type of t0 is not Nat." 
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

typePres' :: Ctx -> Term -> Bool
typePres' ctx tm 
 | (ty1 == ty2) = True
 | otherwise    = False 
  where ty1 = runTypeChecker ctx tm
        tm' = runEval tm
  	ty2 = runTypeChecker ctx tm'
