{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, 
             DeriveDataTypeable,        FlexibleInstances, 
             MultiParamTypeClasses,     FlexibleContexts, 
                                        UndecidableInstances #-} 
------------------------------------------------------------------------
-- This file contains the syntax and all syntax manipulation tools    --
-- for RecCalc.                                                      --
--                                                                    --
-- Course: CSCI-3300 (Fall 2014)                                      --
-- Instructor: Prof. Eades                                            --
------------------------------------------------------------------------

------------------------------------------------------------------------
-- This file exports everything needed to use the syntax of RecCal,  --
-- this prevents the student from having to deal with all of Unbound, --
-- but rather only what we discuss in class.                          --
------------------------------------------------------------------------
module Syntax (module Unbound.LocallyNameless, 
               module Unbound.LocallyNameless.Alpha,
               TmName,
               Type(Nat, Arr),
               Term(Var, Fun, Zero, Suc, App, Rec),               
               replace, 
               n2s) where

import Prelude
import Data.List
import Unbound.LocallyNameless 
import Unbound.LocallyNameless.Alpha

------------------------------------------------------------------------
-- The syntax for RecCalc                                            -- 
------------------------------------------------------------------------
data Type =
    Nat                              -- Nat
  | Arr Type Type                    -- Function types
  deriving (Show,Eq)

type TmName = Name Term              -- Term names
data Term =
    Var TmName                       -- Term variable
  | Fun Type (Bind TmName Term)      -- Lambda abstraction
  | App Term Term                    -- Term application 
  | Zero                             -- Zero
  | Suc Term                         -- Successor
  | Rec Term Term Term               -- Recursor
  deriving (Show)

------------------------------------------------------------------------
-- This derives a bunch of machinery we can use for binding.          --
--                                                                    --
-- If the reader is interested in how this works, please see me       --
-- during my office hours.                                            --
------------------------------------------------------------------------
$(derive [''Term,''Type])
instance Alpha Term
instance Alpha Type

instance Subst Term Type
instance Subst Term Term where
  isvar (Var x) = Just (SubstName x)
  isvar _ = Nothing

------------------------------------------------------------------------
-- The variable substitution functions for RecCalc.                   --
------------------------------------------------------------------------
replace :: Name Term -> Term -> Term -> Term        
replace nm t t' = subst nm t t'

replaceTest :: Fresh m => Name Term -> Term -> Term -> m Term
replaceTest n t t' = return $ replace n t t'

------------------------------------------------------------------------
-- Some helpful redefinitions of Unbound functions.                   --
------------------------------------------------------------------------
n2s :: Name a -> String
n2s = name2String
