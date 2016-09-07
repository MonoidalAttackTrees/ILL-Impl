{-# FlexibleContexts #-}
module TypeCheck where

import Text.Parsec
import Control.Monad.Except

import Syntax
import Pretty
import Parser

------------------------------------------------------------------------
-- Typing contexts                                                    --
------------------------------------------------------------------------
type Ctx = [(TmName, Type)]

emptyCtx :: Ctx
emptyCtx = []

extCtx :: Ctx -> TmName -> Type -> Ctx
extCtx ctx nm ty = (nm, ty) : ctx
------------------------------------------------------------------------
-- Parser for contexts                                                --
------------------------------------------------------------------------
parseCtx :: String -> [(TmName,Type)]
parseCtx str =
   case parse tmCtxParse "" str of
     Left e -> error $ show e
     Right r -> r          
------------------------------------------------------------------------
-- Type checking algorithm                                            --
------------------------------------------------------------------------
typeCheck :: Fresh m => Ctx -> Term -> ExceptT String m Type
typeCheck ctx Unit = do return I
typeCheck ctx (Var t) = do
    case (lookup t ctx) of 
	 Just ty -> return ty 
	 Nothing -> error "Type Error: Term not in the context."
typeCheck ctx (App t1 t2) = do
    t1' <- typeCheck ctx t1
    t2' <- typeCheck ctx t2
    case t1' of
      _ -> undefined

------------------------------------------------------------------------
--                                                 --
------------------------------------------------------------------------
