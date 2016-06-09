{-# LANGUAGE FlexibleContexts #-} 
------------------------------------------------------------------------
-- This file implements the type construction algorithm for RecCalc.  --
--                                                                    --
-- Course: CSCI-3300 (Fall 2014)                                      --
-- Instructor: Prof. Eades                                            --
------------------------------------------------------------------------
module TypeCheck where

import Text.Parsec
import Control.Monad.Trans.Error
import Control.Monad.Trans.Except -- suggested replacement for ErrorT    

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
typeCheck :: Fresh m => Ctx -> Term -> ErrorT String m Type
typeCheck [] _ = undefined
typeCheck ctx Zero = do
        return Nat
typeCheck ctx (Suc t) = do
	x <- typeCheck ctx t
	return x
typeCheck ((tn,ty):c') (Var t)
	| t == tn = return $ ty
 	| otherwise = 
	  do 
    	   x <- typeCheck c' (Var t)
    	   return x        
typeCheck ctx (App t1 t2) = do -- t1 :: T1 -> T2 // t2 :: T1
	t1' <- typeCheck ctx t1
        t2' <- typeCheck ctx t2
	case t1' of
	 Arr t1'' _ -> case t1'' of
			 t2' -> return $ Arr t1'' t2'
			 _ -> error "Type of 1st arg src not the type of 2nd arg."
	 _ -> error "1st arg is not :: Arr Type Type"
        -- return $ Arr t1' t2'
typeCheck ctx (Fun tyA tm) = do
       	 (a, b) <- unbind tm
	 -- let ctx' = extCtx ctx a tyA
	 -- tyB <- typeCheck ctx' b
	 tyB <- typeCheck ((a, tyA):ctx) b
	 return $ Arr tyA tyB
typeCheck ctx (Rec t0 t1 t2) = do
	t0' <- typeCheck ctx t0
	t1' <- typeCheck ctx t1
	t2' <- typeCheck ctx t2
	case t0' of 
	  Nat -> undefined -- continue
	  _ -> error "Type of 1st arg is not Nat."
	case t2' of
	  t1' -> return t1' -- base
	  (Arr t1' (Arr t0' t1'')) -> undefined -- step. T -> Nat -> T
	  _ -> error "Type error in Rec."

-- testUnbind :: (Bind TmName Term) -> (TmName, Term)
-- testUnbind :: (Fun Type (TmName, Term)) -> (TmName, Term)
testUnbind (Fun ty tm) = do
		return unbind tm
-------------------------------------------------------------------------
-- This function makes it easy to run the type checker.                --
-------------------------------------------------------------------------
runTypeChecker :: Ctx -> Term -> Either String Type
runTypeChecker ctx term = runFreshM.runErrorT $ typeCheck ctx term
