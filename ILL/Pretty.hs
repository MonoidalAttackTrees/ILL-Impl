{-# LANGUAGE FlexibleContexts #-}
------------------------------------------------------------------------
-- This file contains the ILL-Impl pretty printers.                   --
------------------------------------------------------------------------
module Pretty where

import Data.List
import Data.Char
import Text.Parsec
import Unbound.LocallyNameless 
import Unbound.LocallyNameless.Alpha
import Control.Monad.Except

import Syntax
import Parser 

------------------------------------------------------------------------
-- prettyType converts a type into a string.                          --
------------------------------------------------------------------------
prettyType :: Fresh m => Type -> ExceptT TypeException m String
prettyType I = return "I"
prettyType (Lolly a b) = do
   a' <- prettyType a
   b' <- prettyType b
   return $ "(" ++ a' ++ ")" ++ " -o " ++ b'
prettyType (Tensor a b) = do
   a' <- prettyType a
   b' <- prettyType b
   return $ a' ++ "(x)" ++ b'
prettyType (Bang ty') = do
  ty <- prettyType ty'
  return $ "!" ++ ty
prettyType _ = throwError InvalidTypeError

------------------------------------------------------------------------
-- prettyTerm converts a term into a string.                          --
------------------------------------------------------------------------
prettyTerm :: Fresh m => Term -> ExceptT TypeException m String
prettyTerm Unit = do
   return "unit"
prettyTerm (Var n) = do
   return.n2s $ n
prettyTerm (BangT t) = do
   t' <- prettyTerm t
   return $ "!" ++ t'
prettyTerm (App t1 t2) = do
   t1' <- prettyTerm t1
   t2' <- prettyTerm t2
   case t1 of
      (App _ _) -> case t2 of
                      Unit -> return $ t1' ++ " " ++ t2'
                      _ -> return $ t1' ++ " (" ++ t2' ++ ")"
      Unit      -> case t2 of 
                      Unit -> return $ t1' ++ " " ++ t2'
                      _ -> return $ t1' ++ " (" ++ t2' ++ ")"
      Var _     -> case t2 of
                      Unit -> return $ t1' ++ " " ++ t2'
                      Var _ -> return $ t1' ++ " " ++ t2'
                      _ -> return $ t1' ++ " (" ++ t2' ++ ")" 
      _         -> case t2 of 
                      Unit -> return $ "(" ++ t1' ++ ") " ++ t2'
                      _ -> return $ "(" ++ t1' ++ ") " ++ "(" ++ t2' ++ ")"
prettyTerm (Lam ty t) = do
   (n,tm) <- unbind t
   tm' <- prettyTerm tm
   tystr <- prettyType ty
   return $ "\\(" ++ (n2s n) ++ ":" ++ tystr ++ ")." ++ tm'
prettyTerm (Tens t1 t2) = do
   t1' <- prettyTerm t1
   t2' <- prettyTerm t2
   return $ "(tens " ++ t1' ++ "," ++ t2' ++ ")"
prettyTerm (LetU t1 t2) = do
   t1' <- prettyTerm t1
   t2' <- prettyTerm t2
   return $ "let " ++ t1' ++ " = " ++ "unit " ++ "in " ++ t2'
prettyTerm (LetT t1 ty t2) = do
   t1' <- prettyTerm t1
   ty' <- prettyType ty
   (x, tm) <- unbind t2
   (y, tm') <- unbind tm
   let x' = n2s x
   let y' = n2s y
   newtm <- prettyTerm tm'
   return $ "let "++x'++"(x)"++ y'++":"++ty'++" = "++t1'++" in "++ newtm
prettyTerm (LetBang t1 ty t2) = do
   t1' <- prettyTerm t1
   ty' <- prettyType ty
   (x, tm) <- unbind t2
   let x' = n2s x
   t2' <- prettyTerm tm
   return $ "let " ++ t1' ++ ":" ++ ty' ++ " = " ++ t2' ++ " in " ++ x'
------------------------------------------------------------------------
-- Testing functions                                                  --
------------------------------------------------------------------------
testPretty parser pretty s = do
  let o = parse parser "" s in  
    case o of
      Left e -> error $ show e
      Right r -> runFreshM (pretty r)

--TODO: Fix helper functions
--testPrettyType :: String -> Either TypeException String
--testPrettyType = testPretty typeParser prettyType

--testPrettyTerm :: String -> Either TypeException String
--testPrettyTerm = testPretty termParser prettyTerm

--runPrettyType :: Type -> Either TypeException String
--runPrettyType = runFreshM.prettyType

--runPrettyTerm :: Term -> Either TypeException String
--runPrettyTerm = runFreshM.prettyTerm
--testPrettyType = runExceptT.runFreshM $ testPretty typeParser prettyType
