{-# LANGUAGE FlexibleContexts #-}

module Testing where

import Syntax
import Parser
import Pretty 

term1 = Fun (Arr Nat Nat) $ bind (s2n "y") Zero
term2 = Var (s2n "x")
term3 = App term1 term2
term4 = Fun (Arr Nat Nat) (bind (s2n "y") $ App term1 term1)

main = do
	let a = runPrettyTerm term1
	let b = runPrettyTerm term2
	let c = runPrettyTerm term3
	let d = runPrettyTerm $ App term1 term2
	let e = runPrettyTerm $ App term1 term1
	let f = runPrettyTerm term4
	putStrLn a
	putStrLn c
	putStrLn d
	putStrLn e
	putStrLn f
