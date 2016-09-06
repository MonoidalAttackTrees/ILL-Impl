module ReplHelp where

hHeader = "Commands available from the ILL REPL: "
hUnfold = " :u :unfold -> Unfolds and prints definitions in queue."
hShow = " :s :show <term>   -> Prints the abstract syntax of a term."
hDump =" :d :dump   -> Prints contents of REPL queue."
hHelp = " :h :help   -> Displays help document."

showHelp = do
  putStrLn hHeader
  putStrLn hUnfold
  putStrLn hShow
  putStrLn hDump
  putStrLn hHelp
