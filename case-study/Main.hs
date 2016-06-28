------------------------------------------------------------------------
-- This file implements the main functions for typing checking,       --
-- evaluating, and testing type preservation of RecCalc programs.    --
--                                                                    --
--                                                                    --
-- Course: CSCI-3300 (Fall 2014)                                      --
-- Instructor: Prof. Eades                                            --
------------------------------------------------------------------------
module Main where

import System.Console.Haskeline

import Parser
import Pretty
import TypeCheck
import Eval

------------------------------------------------------------------------
-- This function prompts the user for a typing context, a term        --
-- context, and a term, and then outputs the term's type with respect --
-- to the input contexts.                                             --
------------------------------------------------------------------------

------------------------------------------------------------------------
-- These functions are used for testing.			      --
------------------------------------------------------------------------
ctx1 :: Ctx
ctx1 = parseCtx $ "a : Nat, b : Nat"

-- \y.y+3
func0 = parseTerm $ "fun y : Nat => suc suc suc y"

-- app \x.x+2 1
appTerm0 = runEval tm
 where tm = parseTerm $ "app (fun x : Nat => (suc suc x)) to suc 0" 

-- app \y.y+3 3
appTerm1 = runEval tm
 where tm = parseTerm $ "app fun y : Nat => suc suc suc y to suc suc suc 0"

recTerm0 = runEval tm
 where tm = parseTerm $ "rec (suc suc 0) with (suc suc suc 0) || fun x : Nat => suc x"

t0 = parseTerm $ "suc suc suc 0" -- recurse 3 times
t1 = parseTerm $ "fun x : Nat => suc x"
t2 = parseTerm $ ""

recTerm1 = runEval tm
 where tm = parseTerm $ "rec (suc 0) with (suc 0) || (fun x : Nat -> Nat => fun y : Nat => suc z)"

recTerm2 = runEval tm
 where tm = parseTerm $ "fun n : Nat => fun m : Nat => rec n with m || (fun r : Nat => fun p : Nat => suc r)"
------------------------------------------------------------------------

mainCheck :: IO ()
mainCheck = do
 putStrLn "Enter a context."
 l <- getLine
 let ctx = parseCtx l
 putStrLn "Enter a term."
 l' <- getLine
 let tm = parseTerm l'
 let ty = case (runTypeChecker ctx tm) of
           Left msg  -> error msg
           Right ty' -> ty'
 putStrLn $ runPrettyType ty
 -- --putStrLn $ read ty :: String

------------------------------------------------------------------------
-- This function prompts the user for a term and then outputs its     --
-- normal form using the evaluator.                                   --
------------------------------------------------------------------------
mainEval :: IO ()
mainEval = do
 putStrLn "Enter a term to evaluate."
 l <- getLine
 let tm  = parseTerm l
 let tm' = runEval tm
 putStrLn $ runPrettyTerm tm'

------------------------------------------------------------------------
-- mainPres prompts the user for a term and then tests that term for  --
-- type preservation.                                                 --
------------------------------------------------------------------------
mainPres :: IO ()
mainPres = do
 putStrLn "Enter a term to check type preservation."
 l <- getLine
 let tm = parseTerm l
 let val = typePres tm
 -- let val = typePres' ctx tm
 putStrLn $ show $ val
