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

term0 :: Term
term0 = parseTerm $ "suc suc suc suc suc 0"

term1 :: Term
term1 = parseTerm $ "0"

term2 :: Term
term2 = parseTerm $ "fun bv : Nat => bdy"

appTerm = runEval tm
 where tm  = parseTerm $ "app (fun bv : Nat => bdy) to b"

recFunct = parseTerm $ "(fun bv1 : Nat => (fun bv2 : (Nat -> Nat) => (suc bv2)))" 

recTerm0 = runEval tm
 where tm = parseTerm $ "rec 0 with (suc suc 0) || (fun bv1 : Nat => (fun bv2 : (Nat -> Nat) => (suc bv2)))"

recTerm1 = runEval tm
 where tm = parseTerm $ "rec (suc suc suc 0) with (suc suc 0) || (fun bv1 : Nat => (fun bv2 : (Nat -> Nat) => (fun bv3 : Nat => (suc 0))))"
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
