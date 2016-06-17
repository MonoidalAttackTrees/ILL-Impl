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
mainCheck :: IO ()
mainCheck = do
 putStrLn "Enter a context."
 l <- getLine
 let ctx = parseCtx l
 putStrLn "Enter a term."
 l' <- getLine
 let tm = parseTerm l'
 let ty = case (runTypeChecker ctx tm) of
           Left left   -> error "Bad type."
           Right right -> right
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
 -- only added a getLine for Ctx because I changed typePres to typePres'
 -- we shall see if a test file makes this easier to test,
 -- otherwise will be a pain
 putStrLn "Enter a context."
 l' <- getLine
 let ctx = parseCtx l'
 let val = typePres' ctx tm
 putStrLn $ show $ val
