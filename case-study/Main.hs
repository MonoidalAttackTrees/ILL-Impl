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
 ctx <- parseCtx line
 putStrLn "Enter a term."
 l' <- getLine
 tm <- parseTerm line
 ty <- runTypeChecker ctx tm
 putStrLn (read ty :: String) 

------------------------------------------------------------------------
-- This function prompts the user for a term and then outputs its     --
-- normal form using the evaluator.                                   --
------------------------------------------------------------------------
mainEval :: IO ()
mainEval = undefined

------------------------------------------------------------------------
-- mainPres prompts the user for a term and then tests that term for  --
-- type preservation.                                                 --
------------------------------------------------------------------------
mainPres :: IO ()
mainPres = undefined
