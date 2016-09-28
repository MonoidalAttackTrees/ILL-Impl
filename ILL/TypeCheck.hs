{-# LANGUAGE FlexibleContexts #-}
module TypeCheck where

import Text.Parsec
import Control.Monad.Except
import Data.List

import Syntax
import Pretty
import Parser
------------------------------------------------------------------------
-- TypeError handles error data & future error handling               --
------------------------------------------------------------------------
data TypeError =
    NonEmptyCtxError
  | EmptyCtxError
  | VarError -- expand
  | AppSrcError
  -- TODO: Implement error types for TypeCheck cases
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
-- Free Variable Collection                                           --
------------------------------------------------------------------------
fv :: Fresh m => Term -> m [TmName]
fv Unit = return []
fv (Var t) = return [t]
fv (LetU t1 t2) = do
   t1' <- fv t1
   t2' <- fv t2
   return $ t1' ++ t2'
fv (Tens t1 t2) = do
   t1' <- fv t1
   t2' <- fv t2
   return $ t1' ++ t2'
fv (Lam ty b) = do
   (n,tm) <- unbind b
   t <- fv tm
   return $ t \\ [n]
fv (App t1 t2) = do
   t1' <- fv t1
   t2' <- fv t2
   return $ t1' ++ t2'
fv (LetT t ty b) = do
   (n,tm) <- unbind b
   (n',tm') <- unbind tm
   t1 <- fv t
   t2 <- fv tm'
   return $ t1 ++ (t2 \\ [n,n'])
fv (LetBang t ty b) = do
   t1 <- fv t
   (n,tm) <- unbind b
   t2 <- fv tm
   return $ t1 ++ (t2 \\ [n])
fv (BangT t) = do
   t' <- fv t
   return t'
------------------------------------------------------------------------
-- Splitting linear contexts. Takes list of free variables and a      --
-- context, creating the subcontext that contains those free variables--
------------------------------------------------------------------------
-- subCtx :: Fresh m => Ctx -> [TmName] -> m Ctx
-- subCtx [] n = return emptyCtx
-- subCtx ctx (n:ns) = do
--   case (lookup n ctx) of
--     Just ty -> do
--       let ctx' = (n,ty) : ctx
--       c <- subCtx ctx' ns
--       return c
--     _ -> return emptyCtx

subCtx :: [TmName] -> Ctx -> Ctx
subCtx n [] = emptyCtx
subCtx (n:ns) ctx = do
  case (lookup n ctx) of
    Just ty -> subCtx ns ((n,ty):ctx) 
    Nothing -> subCtx ns ctx 
------------------------------------------------------------------------
-- Compose free variable collection & split context                   --
------------------------------------------------------------------------
splitCtx :: Fresh m => Ctx -> Term -> m Ctx
splitCtx ctx tm = do
  tms <- fv tm
  return $ subCtx tms ctx
-- splitCtx ctx tm = fv tm . subCtx ctx
-- splitCtx ctx tm = (fv tm) >>= subCtx ctx
------------------------------------------------------------------------
-- Type checking algorithm                                            --
-- c1 (GAMMA) denotes intuitionistic context                          --
-- c2 (DELTA) denotes linear context                                  --
------------------------------------------------------------------------
typeCheck :: Fresh m => Ctx -> Ctx -> Term -> ExceptT TypeError m Type
typeCheck c1 [] Unit = return I
typeCheck c1 _ Unit = error "" -- NonEmptyCtxError
-- intuitionistic Var
typeCheck c1 [] (Var t) = do
  ctx <- splitCtx c1 $ Var t
  case (lookup t ctx) of
    Nothing -> error ""
    Just ty -> return ty
-- linear Var
typeCheck _ c2 (Var t) = do
  ctx <- splitCtx c2 $ Var t
  if (length ctx == 1)
     then undefined          -- TODO
     else error ""
typeCheck c1 [] (BangT t) = do
  ty <- typeCheck c1 [] t
  return $ Bang ty
typeCheck c1 _ (BangT t) = error "" -- NonEmptyCtxError
typeCheck c1 c2 (LetU t1 t2) = do
  c1' <- splitCtx c1 t1
  c2' <- splitCtx c2 t2
  undefined
------------------------------------------------------------------------
--                                                                    --
------------------------------------------------------------------------
runTypeChecker :: Ctx -> Ctx -> Term -> Either TypeError Type
runTypeChecker c1 c2 term = runFreshM.runExceptT $ typeCheck c1 c2 term
