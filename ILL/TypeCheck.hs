{-# LANGUAGE FlexibleContexts #-}
module TypeCheck where

import Text.Parsec
import Control.Monad.Except

import Syntax
import Pretty
import Parser
import ILLError
------------------------------------------------------------------------
-- Typing contexts                                                    --
------------------------------------------------------------------------
type Ctx = [(TmName, Type)]

emptyCtx :: Ctx
emptyCtx = []

extCtx :: Ctx -> TmName -> Type -> Ctx
extCtx ctx nm ty = (nm, ty) : ctx

remCtx :: Ctx -> TmName -> Type -> Ctx
remCtx [] nm ty = []
remCtx (c:ctx) nm ty = case (lookup nm (c:ctx)) of
                    Nothing -> remCtx ctx nm ty
                    _ -> ctx
------------------------------------------------------------------------
-- Parser for contexts                                                --
------------------------------------------------------------------------
parseCtx :: String -> [(TmName,Type)]
parseCtx str =
   case parse tmCtxParse "" str of
     Left e -> error $ show e
     Right r -> r
------------------------------------------------------------------------
-- Free Variable Collection                                           --
------------------------------------------------------------------------
--getFV :: Ctx -> Ctx -> Term -> Ctx
getFV c1 c2 Unit = c1 ++ []
getFV c1 c2 (BangT t) = getFV c1 c2 t
getFV c1 c2 (Var tmn@(t)) = undefined
getFV c1 c2 (App t1 t2) = getFV c1 c2 t1 ++ getFV c1 c2 t2
getFV c1 c2 (Tens t1 t2) = getFV c1 c2 t1 ++ getFV c1 c2 t2
getFV c1 c2 (LetU t1 t2) = getFV c1 c2 t1 ++ getFV c1 c2 t2
getFV c1 c2 (Lam ty t) = undefined
------------------------------------------------------------------------
-- Type checking algorithm                                            --
------------------------------------------------------------------------
typeCheck :: Fresh m => Ctx -> Ctx -> Term -> ExceptT TypeError m Type
typeCheck c1 c2 Unit = return I
typeCheck c1 c2 (Var t) = undefined
-- Two separate Var cases?
typeCheck c1 c2 (BangT t) = do
    ty <- typeCheck c1 c2 t
    return $ Bang ty

------------------------------------------------------------------------
--                                                                    --
------------------------------------------------------------------------
runTypeChecker :: Ctx -> Ctx -> Term -> Either TypeError Type
runTypeChecker c1 c2 term = runFreshM.runExceptT $ typeCheck c1 c2 term
