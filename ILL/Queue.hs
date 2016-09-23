{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------
-- Functional Queues                                                    --
--                                                                      --
-- These are based on the definition found in Okasaki's "Purely         --
-- Functional Data Structures".                                         --
--                                                                      --
-- One unique tool this library comes with is a structural fixpoint     --
-- combinator over queues to make complex function definitions easier.  --
--------------------------------------------------------------------------

module Queue where

import Prelude hiding (lookup)
-- imports for testing
import Syntax
import Parser
import Pretty
import TypeCheck
-- remove imports after recursive lookup

-- redundant Qelm (found in Repl) for testing purposes
type Qelm = (TmName, Term)

data Queue a = Queue [a] [a]

toListQ :: Queue a -> [a]
toListQ (Queue f r) = f ++ (reverse r)

-- This instance allows us to use Haskell's built in foldl and foldr.
instance Foldable Queue where
    foldMap m = (foldMap m).toListQ

instance Show a => Show (Queue a) where
    show = show.toListQ
                
emptyQ :: Queue a
emptyQ = Queue [] []
           
headQ :: Queue a -> a
headQ (Queue (x:xs) _) = x

queue :: [a] -> [a] -> Queue a
queue [] r = Queue (reverse r) []
queue f r = Queue f r

snoc :: Queue a -> a -> Queue a
snoc (Queue f r) x = queue f (x:r)

tailQ :: Queue a -> Queue a
tailQ (Queue (x:f) r) = queue f r
                       
mapQ :: (a -> b) -> Queue a -> Queue b
mapQ m (Queue f r) = Queue (map m f) (map m r)

-- lookup for Queues - fixpoint recursion version coming soon
lookup :: Eq a => Queue a -> a -> a
lookup q a =
  case (lookup' q a) of
       Just x -> x
       Nothing -> undefined

lookup' :: Eq a => Queue a -> a -> Maybe a
lookup' (Queue [] []) _ = Nothing
lookup' (Queue [] (l:ls)) t =
  if (t == l) then Just l
  else (lookup' (Queue [] ls) t) 
lookup' (Queue (h:_) []) t =
   if (t == h) then Just h
   else Nothing
lookup' (Queue (h:_) (l:ls)) t =
   if (t == h) then Just h
   else if (t == l) then Just l
   else
     case (lookup' (Queue [] ls) t) of
      Nothing -> Nothing
      Just x -> Just x

-- fixQ function to replace Queue lookup/lookup'
lookup'' q@(Queue (l:ls) l2) t = (fixQ q (return ()) (\x q' r -> (putStrLn.show $ x)))

lookupStep (l:ls) q t = undefined

-- Test queues
term1 = parseTerm "\\(x:I).unit"

qu :: (Queue Qelm)
qu = (queue (((s2n "x"), term1) (((s2n "y"), term1), term1)))

-- The following fixpoint operation makes it easier to do structural
-- recursion over queues.
--
--   fixQ q baseCase stepCase
--
-- The stepcase is a function that takes in the head of q, the tail of
-- q, and the recursive call.
--
-- For example, if we have the following queue:
--
-- testQ = snoc (snoc (snoc (snoc (snoc (snoc (snoc emptyQ 1) 2) 3) 4) 5) 6) 7
--
-- then we can output it to STDOUT as follows:
--
-- fixQ testQ (return ()) (\x q r -> (putStrLn.show $ x) >> r)

fixQ :: Queue a -> b -> (a -> Queue a -> b -> b) -> b
fixQ (Queue [] []) base _ = base
fixQ (Queue [] r) base step = fixQ (queue [] r) base step
fixQ (Queue (x:f) r) base step =
    let q = Queue f r
     in step x q (fixQ q base step)
