{-# LANGUAGE TemplateHaskell,       FlexibleInstances,
    	     MultiParamTypeClasses, FlexibleContexts, 
             UndecidableInstances                      #-}
------------------------------------------------------------------------
-- This file contains the syntax and syntax manipulators of ILL-Impl. --
------------------------------------------------------------------------

module Syntax (module Unbound.LocallyNameless,
               module Unbound.LocallyNameless.Alpha,
               Type(..),
               Term(..),
               TmName,
               replace,
               replaceTest,
               n2s) where

import Prelude
import Data.List
import Unbound.LocallyNameless 
import Unbound.LocallyNameless.Alpha

data Type =
    I 
  | Lolly Type Type
  | Tensor Type Type
  | Bang Type
  deriving (Show, Eq)

type TmName = Name Term

data Term = 
    Var TmName
  | BangT Term 
  | Lam Type (Bind TmName Term)
  | App Term Term
  | Tens Term Term
  | Unit
  | LetU Term Term
  | LetT Term Type (Bind TmName (Bind TmName Term))
  | LetBang Term Type (Bind TmName Term)
  deriving (Show)

$(derive [''Term,''Type])
instance Alpha Term
instance Alpha Type

instance Subst Term Type
instance Subst Term Term where
  isvar (Var x) = Just (SubstName x)
  isvar _ = Nothing

replace :: Name Term -> Term -> Term -> Term        
replace nm t t' = subst nm t t'

replaceTest :: Fresh m => Name Term -> Term -> Term -> m Term
replaceTest n t t' = return $ replace n t t'

-- NameToString

n2s :: Name a -> String
n2s = name2String
