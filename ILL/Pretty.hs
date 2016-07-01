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
   return $ a' ++ " -o " ++ b'
prettyType (TensorTy a b) = do
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
   let n' = n2s n
   return n'
prettyTerm (App t1 t2) = do
   t1' <- prettyTerm t1
   t2' <- prettyTerm t2
   return $ t1' ++ " " ++ t2'
prettyTerm (Lam ty t) = do
   (n,tm) <- unbind t
   str <- prettyTerm tm
   tystr <- prettyType ty
   return $ "\\" ++ (n2s n) ++ " : " ++ tystr ++ "." ++ "(" ++ str ++ ")"
prettyTerm (Tensor t1 t2) = do
   t1' <- prettyTerm t1
   t2' <- prettyTerm t2
   return $ t1' ++ " (x) " ++ t2'
prettyTerm (LetU t1 t2) = do
   t1' <- prettyTerm t1
   t2' <- prettyTerm t2
   return $ "let " ++ t1' ++ " be " ++ "unit " ++ "in " ++ t2'
prettyTerm (LetT t1 t2) = undefined

