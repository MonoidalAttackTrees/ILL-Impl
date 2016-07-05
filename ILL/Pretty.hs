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

import Syntax
import Parser 

------------------------------------------------------------------------
-- prettyType converts a type into a string.                          --
------------------------------------------------------------------------
prettyType :: Fresh m => Type -> m String
prettyType I = return "I"
prettyType (Lolly a b) = do
   a' <- prettyType a
   b' <- prettyType b
   case a of
      (Lolly _ _) -> return $ "(" ++ a' ++ ")" ++ " -o " ++ b'
      _ -> return $ a' ++ " -o " ++ b'
prettyType (Tensor a b) = do
   a' <- prettyType a
   b' <- prettyType b
   return $ a' ++ " (x) " ++ b'

------------------------------------------------------------------------
-- prettyTerm converts a term into a string.                          --
------------------------------------------------------------------------
prettyTerm :: Fresh m => Term -> m String
prettyTerm Unit = do
   return "unit"
prettyTerm (Var n) = do
   return.n2s $ n
prettyTerm (App t1 t2) = do
   t1' <- prettyTerm t1
   t2' <- prettyTerm t2
   case t1 of
      (Lam _ _) -> return $ "(" ++ t1' ++ ")" ++ " " ++ t2'
      _ -> return $ t1' ++ " " ++ t2'
prettyTerm (Lam ty t) = do
   (n,tm) <- unbind t
   str <- prettyTerm tm
   tystr <- prettyType ty
   return $ "\\" ++ (n2s n) ++ " : " ++ tystr ++ "." ++ "(" ++ str ++ ")"
prettyTerm (Tens t1 t2) = do
   t1' <- prettyTerm t1
   t2' <- prettyTerm t2
   return $ t1' ++ " (x) " ++ t2'
prettyTerm (LetU t1 t2) = do
   t1' <- prettyTerm t1
   t2' <- prettyTerm t2
   return $ "let " ++ t1' ++ " be " ++ "unit " ++ "in " ++ t2'
prettyTerm (LetT t1 t2) = do
   t1' <- prettyTerm t1
   (x, tm) <- unbind t2
   (y, tm') <- unbind tm
   let x' = n2s x
   let y' = n2s y
   newtm <- prettyTerm tm'
   return $ "let " ++ t1' ++ " be " ++ x' ++ "(x)" ++ y' ++ " in " ++ newtm
