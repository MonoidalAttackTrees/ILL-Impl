------------------------------------------------------------------------
-- This file implements the type construction algorithm for RecCalc.  --
--                                                                    --
-- Course: CSCI-3300 (Fall 2014)                                      --
-- Instructor: Prof. Eades                                            --
------------------------------------------------------------------------
module TypeCheck where

import Text.Parsec
import Control.Monad.Trans.Error    

import Syntax
import Parser
import Pretty 

------------------------------------------------------------------------
-- Typing Contexts.                                                   --
------------------------------------------------------------------------
type Ctx = [(TmName, Type)]

emptyCtx :: Ctx
emptyCtx = []

-------------------------------------------------------------------------
-- Extension function for contexts.  It adds a term name and a         --
-- corresponding type to the context.                                  --
-------------------------------------------------------------------------
extCtx :: Ctx -> TmName -> Type -> Ctx
extCtx ctx nm ty = (nm, ty) : ctx

-------------------------------------------------------------------------
-- This function parses a string corresponding to a context, and then  --
-- parses it into an actual context.                                   --
-------------------------------------------------------------------------
parseCtx :: String -> [ (TmName, Type) ]
parseCtx str = 
    case parse tmCtxParse "" str of
      Left e  -> error $ show e
      Right r -> r

-------------------------------------------------------------------------
-- This is the type construction function.  Given a context and a term --
-- this function will either return the type the input term inhabits   --
-- or throws an error.                                                 --
-------------------------------------------------------------------------
typeCheck :: Fresh m => Ctx -> Term -> ErrorT String m Type
typeCheck [] _ = undefined
typeCheck ((nm, ty):ctx) tm = undefined

-------------------------------------------------------------------------
-- This function makes it easy to run the type checker.                --
-------------------------------------------------------------------------
runTypeChecker :: Ctx -> Term -> Either String Type
runTypeChecker ctx term = runFreshM.runErrorT $ typeCheck ctx term
