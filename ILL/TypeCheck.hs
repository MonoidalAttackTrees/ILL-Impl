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
typeCheck ctx Unit = return I
typeCheck ctx (Var t) = do
    case (lookup t ctx) of 
	 Just ty -> return ty 
	 Nothing -> error "Type Error: Term not in context."
typeCheck ctx (BangT t) = do
    t' <- typeCheck ctx t
    return $ Bang t'
typeCheck ctx (Tens t1 t2) = do
    t1' <- typeCheck ctx t1
    t2' <- typeCheck ctx t2
    return $ Tensor t1' t2'
typeCheck ctx (Lam ty tm) = do
    (a,b) <- unbind tm
    let ctx' = extCtx ctx a ty
    tyB <- typeCheck ctx' b
    return $ Lolly ty tyB
typeCheck ctx (App t1 t2) = do
    t1' <- typeCheck ctx t1
    t2' <- typeCheck ctx t2
    case t1' of
      (Lolly x y) -> case t2' of
                        x' -> if x' == x then return x
                              else error "Type Error: Second argument is not the \
                                         \same type of first argument's source."
      _ -> error "Type Error: First arg is not type Lolly."
typeCheck ctx (LetU t1 t2) = do
    t1' <- typeCheck ctx t1 -- may revise LetU's t1 from a term to just Var
    t2' <- typeCheck ctx t2
    case t1' of
         I -> case t2' of
                   ty -> return ty
         _ -> error "Type Error: First term is not type Unit."
typeCheck ctx (LetT t1 ty t2) = do
    t1' <- typeCheck ctx t1
    (a,b) <- unbind t2
    (a',b') <- unbind b
    let ctx' = extCtx ctx a ty
    -- unbind the nested terms, add other one to ctx
    t2' <- typeCheck ctx' b'
    case t1' of
         (Tensor t1' t2') -> return t2'
         _ -> error "Type Error: First term is not type Tensor."
typeCheck ctx (LetBang t1 ty t2) = do
    t1' <- typeCheck ctx t1
    (a,b) <- unbind t2
    ty2' <- typeCheck ctx b
    case t1' of
         (Bang t) -> return $ ty2'
         _ -> error "Type Error: First argument is not of type Bang"
------------------------------------------------------------------------
--                                                                    --
------------------------------------------------------------------------
runTypeChecker :: Ctx -> Term -> Either String Type
runTypeChecker ctx term = runFreshM.runExceptT $ typeCheck ctx term
