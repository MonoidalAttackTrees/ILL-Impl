{-# LANGUAGE FlexibleContexts #-}

module Testing where

import Syntax
import Parser
import Pretty
import TypeCheck 

badterm1 = Fun (Arr Nat Nat) $ bind (s2n "y") Zero
-- badterm2 = Fun (Arr Nat Nat) (bind (s2n "y") $ App term1 term1)

term_y = Var (s2n "y")
term1 = Fun Nat $ bind (s2n "x") term_y
term2 = Fun (Arr Nat Nat) (bind (s2n "z") term1)
term3 = Rec (Suc(Suc(Zero))) term2 term1

ctx = extCtx emptyCtx nm ty 
       where 
        (nm,ty) = ((fst $ head $ parseCtx $ runPrettyTerm term1),
                   (snd $ head $ parseCtx $ runPrettyTerm term1))
	
main = do
	let a = runPrettyTerm term1
	putStrLn a
	let b = runPrettyTerm term2
	putStrLn b
	let c = runPrettyTerm term3
	putStrLn c

	
