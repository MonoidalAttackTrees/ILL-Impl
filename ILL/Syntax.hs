{-# LANGUAGE TemplateHaskell,       FlexibleInstances,
    	     MultiParamTypeClasses, FlexibleContexts, 
             UndecidableInstances                      #-}
------------------------------------------------------------------------
-- This file contains the syntax and syntax manipulators of ILL-Impl. --
------------------------------------------------------------------------

module Syntax (module Unbound.LocallyNameless,
               module Unbound.LocallyNameless.Alpha,
	       TmName,
               Type(..),
               Term(..),
               replace,
               replaceTest,
               n2s) where

import Prelude
import Data.List
import Unbound.LocallyNameless 
import Unbound.LocallyNameless.Alpha

data Type =
    UnitTy 
  | Lolly Type Type
  | TensorTy Type Type
  deriving (Show, Eq)

type TmName = Name Term

data Term = 
    Var TmName
  | Lam Type (Bind TmName Term)
  | App Term Term
  | Tensor Term Term
  | Unit
  | Let Term Term Term
  | Derelict Term
  | Discard Term Term
  | Promote [Term] [Term] Term
  | Copy Term (Term,Term) Term
  deriving (Show)

-- Machinery from RecCalc syntax

$(derive [''Term,''Type])
instance Alpha Term
instance Alpha Type

instance Subst Term Type
instance Subst Term Term where
  isvar (Var x) = Just (SubstName x)
  isvar _ = Nothing

-- Variable subst from RecCalc

replace :: Name Term -> Term -> Term -> Term        
replace nm t t' = subst nm t t'

replaceTest :: Fresh m => Name Term -> Term -> Term -> m Term
replaceTest n t t' = return $ replace n t t'

-- NameToString

n2s :: Name a -> String
n2s = name2String
