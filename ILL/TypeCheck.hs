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
    NonEmptyCtxException
  | EmptyCtxException
  | TermNotInCtxException
  | VarException
  | AppSrcException
  | InvalidArgException
  | InvalidTypeException
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
subCtx :: [TmName] -> Ctx -> Maybe Ctx
subCtx n [] = Just emptyCtx
subCtx (n:ns) c = do
  ty <- lookup n c
  (subCtx ns c) >>= (\ctx' -> return ((n,ty):ctx'))
------------------------------------------------------------------------
-- Create disjoint sub-contexts from gamma and delta                      --
------------------------------------------------------------------------
disjointCtx :: Fresh m => Ctx -> Term -> ExceptT TypeError m Ctx
disjointCtx ctx tm = do
  tms <- fv tm
  let mctx' = subCtx tms ctx
   in case mctx' of
        Just ctx' -> return ctx'
        Nothing -> throwError TermNotInCtxException
------------------------------------------------------------------------
-- Type checking algorithm                                            --
-- c1 (GAMMA) denotes intuitionistic context                          --
-- c2 (DELTA) denotes linear context                                  --
------------------------------------------------------------------------
typeCheck :: Fresh m => Ctx -> Ctx -> Term -> ExceptT TypeError m Type
typeCheck g [] Unit = return I
typeCheck g _ Unit = throwError NonEmptyCtxException
typeCheck g [] (Var t) = do
  ctx <- disjointCtx g $ Var t
  case (lookup t ctx) of
    Nothing -> throwError VarException
    Just ty -> return ty
typeCheck _ [(x,ty)] (Var y) = do
  if (x == y)
     then return $ ty
     else throwError VarException
typeCheck c1 [] (BangT t) = do
  ty <- typeCheck c1 [] t
  return $ Bang ty
typeCheck c1 _ (BangT t) = throwError NonEmptyCtxException
typeCheck g d (Tens t u) = do
  d1 <- disjointCtx d t
  d2 <- disjointCtx d u
  ty1 <- typeCheck g d1 t
  ty2 <- typeCheck g d2 u
  return $ Tensor ty1 ty2
typeCheck g d (LetU u t) = do
  d1 <- disjointCtx d u
  d2 <- disjointCtx d t
  ty1 <- typeCheck g d1 u
  ty2 <- typeCheck g d2 t
  if (ty1 == I)
    then return ty2
    else throwError $ InvalidTypeException
typeCheck g d (LetT u (Tensor a b) t'') = do
  (n',t') <- unbind t''
  (n,t) <- unbind t'
  d1 <- disjointCtx d u
  d2 <- disjointCtx d t
  tyu <- typeCheck g d1 u
  tyt <- typeCheck g d2 t
  case tyu of
    (Tensor a' b') -> return tyt
    _ -> throwError InvalidTypeException
typeCheck g d (LetT u _ t) = throwError InvalidTypeException
typeCheck g d (LetBang u (Bang ty) t') = do
  (n,t) <- unbind t'
  d1 <- disjointCtx d u
  d2 <- disjointCtx d t
  g2 <- disjointCtx g t
  tyu <- typeCheck g d1 u
  case tyu of
    (Bang ty) -> (typeCheck g2 d2 t) >>= (\tyt -> return tyt)
    _ -> throwError InvalidTypeException
typeCheck g d (LetBang u _ t') = throwError InvalidTypeException 
typeCheck g d (Lam ty t) = do
  (n,t') <- unbind t
  d' <- disjointCtx d t'
  ty2 <- typeCheck g d' t'
  return $ Lolly ty ty2
typeCheck g d (App u t) = do
  d1 <- disjointCtx d u
  d2 <- disjointCtx d t
  tyu <- typeCheck g d1 u
  tyt <- typeCheck g d2 t
  case tyu of
   (Lolly a b) -> if (tyt == a)
                  then return tyt
                  else throwError AppSrcException
   _ -> throwError AppSrcException
------------------------------------------------------------------------
--                                                                    --
------------------------------------------------------------------------
runTypeChecker :: Ctx -> Ctx -> Term -> Either TypeError Type
runTypeChecker c1 c2 term = runFreshM.runExceptT $ typeCheck c1 c2 term
