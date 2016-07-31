module Repl where

import Control.Monad.State
import Control.Monad.Except
import System.Console.Haskeline
import System.Console.Haskeline.MonadException
import System.Exit
import Unbound.LocallyNameless.Subst

import Syntax
import Parser
import Pretty
import Queue

type Qelm = (TmName, Term)
type REPLStateIO = StateT (Queue Qelm) IO

instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in fmap (flip runStateT s) $ f run'

deQelm :: Qelm -> (TmName, Term)
deQelm (tmn, tm) = (tmn, tm)

deQelmL :: [Qelm] -> [(TmName, Term)]
deQelmL q = map deQelm q

tmL :: [(TmName, Term)] -> [TmName]
tmL ((x,y):ls) = x:(tmL ls) 

io :: IO a -> REPLStateIO a
io i = liftIO i

pop :: REPLStateIO (TmName, Term)
pop = get >>= return.headQ

push :: Qelm -> REPLStateIO ()
push t = get >>= put.(`snoc` t) 
         
unfoldDefsInTerm :: (Queue Qelm) -> Term -> Term
unfoldDefsInTerm q t =
    let uq = toListQ $ unfoldQueue q
     in substs uq t

unfoldQueue :: (Queue Qelm) -> (Queue Qelm)
unfoldQueue q = fixQ q emptyQ step
 where
   step e@(x,t) _ r = (mapQ (substDef x t) r) `snoc` e
    where
      substDef :: Name Term -> Term -> Qelm -> Qelm
      substDef x t (y, t') = (y, subst x t t')

checkQ :: (Queue Qelm) -> TmName -> Term
checkQ q tmn =
   case (checkQ' q tmn) of
     Left lf -> Var (s2n "ErrorVar") -- temporary solution for error handling
     Right rt -> rt
   where
      l = toListQ $ unfoldQueue q

checkQ' :: (Queue Qelm) -> TmName -> Either String Term
checkQ' q tmn =
   case (lookup tmn l) of
      Just x -> Right x
      Nothing -> Left "Errrr" 
   where
      l = toListQ $ unfoldQueue q

handleCMD :: String -> REPLStateIO()
handleCMD "" = return ()
handleCMD s =
    case (parseLine s) of
      Left msg -> io $ putStrLn msg
      Right l -> handleLine l
  where
    handleLine :: REPLExpr -> REPLStateIO()
    handleLine (Let x t) = push (x,t)
    handleLine (ShowAST t) = io.putStrLn.show $ t
    handleLine (Unfold t) = get >>= (\defs -> io.putStrLn.runPrettyTerm $ unfoldDefsInTerm defs t)
    handleLine (CallTerm tm) = get >>= (\q -> io.putStrLn.runPrettyTerm $ checkQ q tm)
    handleLine DumpState = get >>= io.print.(mapQ prettyDef)
     where
       prettyDef (x, t) = "let " ++ (n2s x) ++ " = " ++ (runPrettyTerm t)

banner :: String
banner = "Welcome to Ill, an Intuitionistic Linear Logic programming language!\n\n"

main = do
    putStr banner
    evalStateT (runInputT defaultSettings loop) emptyQ
      where
         loop :: InputT REPLStateIO ()
         loop = do
                 minput <- getInputLine "Ill> "
                 case minput of
                     Nothing -> return ()
                     Just input | input == ":q" || input == ":quit" ->
                                  liftIO $ putStrLn "Exiting Ill." >> return ()
                                | otherwise -> (lift.handleCMD $ input) >> loop
    
