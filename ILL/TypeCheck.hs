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

infixr 5 |:|
(|:|) :: (TmName,Type) -> Ctx -> Ctx
(x,t) |:| g = extCtx g x t

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
-- Create disjoint sub-contexts from gamma and delta                  --
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
-- g (GAMMA) denotes intuitionistic context                           --
-- d (DELTA) denotes linear context                                   --
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
-- Rule: ! - I
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
typeCheck g d (LetU t u) = do
  d1 <- disjointCtx d t
  d2 <- disjointCtx d u
  ty1 <- typeCheck g d1 t
  ty2 <- typeCheck g d2 u
  if (ty1 == I)
    then return ty2
    else throwError $ InvalidTypeException
typeCheck g d (LetT u (Tensor a b) t'') = do
  (x,t') <- unbind t''
  (y,t) <- unbind t'
  d1 <- disjointCtx d u
  d2 <- disjointCtx d t
  tyu <- typeCheck g d1 u
  tyt <- typeCheck g ((x,a) |:| (y,b) |:| d2) t
  case tyu of
    (Tensor a' b') ->
        if ((a',b') == (a,b))
        then return tyt
        else throwError InvalidTypeException
    _ -> throwError InvalidTypeException
typeCheck g d (LetT u _ t) = throwError InvalidTypeException
typeCheck g d (LetBang u (ty@(Bang a)) t') = do
  (x,t) <- unbind t'
  d1 <- disjointCtx d u
  d2 <- disjointCtx d t 
  tyu <- typeCheck g d1 u  
  case tyu of
    (Bang a') ->
        if a' == a
        then typeCheck ((x,ty) |:| g) d2 t
        else throwError InvalidTypeException
    _ -> throwError InvalidTypeException
typeCheck g d (LetBang u _ t') = throwError InvalidTypeException 
typeCheck g d (Lam ty t') = do
  (x,t) <- unbind t'
  ty2 <- typeCheck g ((x,ty) |:| d) t
  return $ Lolly ty ty2
typeCheck g d (App u t) = do
  d1 <- disjointCtx d u
  d2 <- disjointCtx d t
  tyu <- typeCheck g d1 u
  tyt <- typeCheck g d2 t
  case tyu of
   (Lolly a b) -> if (tyt == a)
                  then return b
                  else throwError AppSrcException
   _ -> throwError AppSrcException
------------------------------------------------------------------------
--                                                                    --
------------------------------------------------------------------------
constructType :: Term -> Either TypeError Type
constructType term =
    runFreshM.runExceptT $ typeCheck emptyCtx emptyCtx term
