{-# LANGUAGE FlexibleContexts #-} 
------------------------------------------------------------------------
-- This file implements the type construction algorithm for RecCalc.  --
--                                                                    --
-- Course: CSCI-3300 (Fall 2014)                                      --
-- Instructor: Prof. Eades                                            --
------------------------------------------------------------------------
module TypeCheck where

import Text.Parsec
import Control.Monad.Except

import Syntax
import Parser
import Pretty
-- import Testing

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
typeCheck :: Fresh m => Ctx -> Term -> ExceptT String m Type
typeCheck [] _ = undefined -- is an empty context an error? 
typeCheck ctx Zero = do
        return Nat
typeCheck ctx (Suc t) = do
	x <- typeCheck ctx t
	return x
typeCheck ctx (Var t) = do
	case (lookup t ctx) of 
	 Just ty -> return ty 
	 Nothing -> error "Term not in the context!"
--typeCheck ((tn,ty):c') (Var t)
	-- | t == tn = return $ ty
 	-- | otherwise = 
	--  do 
    	--   x <- typeCheck c' (Var t)
    	--   return x        
typeCheck ctx (App t1 t2) = do
	ty1 <- typeCheck ctx t1
        ty2 <- typeCheck ctx t2
	case ty1 of
	 (Arr ty1' ty2') -> 
		case ty1' of
		  ty1 -> return ty2'
	 _ -> error "1st arg is not a function."
typeCheck ctx (Fun tyA tm) = do
       	 (a,b) <- unbind tm
	 let ctx' = extCtx ctx a tyA
	 tyB <- typeCheck ctx' b
	 -- tyB <- typeCheck ((a,tyA):ctx) b
	 return $ Arr tyA tyB
typeCheck ctx (Rec t0 t1 t2) = do
	ty0 <- typeCheck ctx t0
	ty1 <- typeCheck ctx t1
	ty2 <- typeCheck ctx t2
	case ty0 of
	 Nat -> case ty2 of
		 (Arr ty1 (Arr ty0 ty1')) -> 
		  case ty1' of
		   ty1 -> return ty1
	 	 
-------------------------------------------------------------------------
-- This function makes it easy to run the type checker.                --
-------------------------------------------------------------------------
runTypeChecker :: Ctx -> Term -> Either String Type
runTypeChecker ctx term = runFreshM.runExceptT $ typeCheck ctx term
