module Testing where

import Syntax
import Parser
import Pretty

------------------------------------------------------------------------
-- Lambda testing                              			      --
------------------------------------------------------------------------

lamtest0 = parseTester lamParse "\\(x:I).y"
-- successful

lamtest1 = parseTester lamParse "\\(x:I(x)I).y"
-- successful

lamtest' = parseTester lamParse "\\(x:I(x)I).(tens y z)"

lamtest2 = parseTester lamParse "\\(x:I-oI).y"
-- unsuccessful, lolly type wants an explicit space before '-'
-- feature? y/n

lamtest3 = parseTester lamParse "\\(x:I -o I).y"
-- successful

lamtest4 = parseTester lamParse "\\(x:I -o I (x) I -o I).y"
-- unparenthesized forces '\x:I -o (I -o (I (x) I) I).y

lamtest5 = parseTester lamParse "\\(x:I -o I(x)I -o I).y"
-- space is not an issue for tensor

lamtest6 = parseTester lamParse "\\(x:(I -o I)(x)(I -o I)).y"
-- successful

lamtest7 = parseTester lamParse "\\(x:(I -o I)(x)(I -o I)).y z"
-- successful application

lamtest8 = parseTester lamParse "\\(x:(I -o I)(x)(I -o I)).(y unit) (x) unit"
-- unsuccessful; bad parse

lamtest8' = parseTester lamParse "\\(x:(I -o I)(x)(I -o I)).y unit(x)unit"

lamtest9 = parseTester lamParse "\\(x:(I -o I)(x)(I -o I)).y (unit (x) unit)"
-- unsuccesful; tries to parse App Unit (var x)

lamtest10 = parseTester lamParse "\\(x:(I -o I)(x)(I -o I)).y (unit(x)unit)"

------------------------------------------------------------------------
-- New Tensor term testing                 			      --
------------------------------------------------------------------------
tens0 = parseTester tensParse "tens x,y"

tens1 = parseTester tensParse "tens unit,unit"

tens2 = parseTester tensParse "tens unit,tens unit,unit"
-- success

tens3 = parseTester tensParse "tens x y,a b"
-- success; handles nested App well

tens4 = parseTester tensParse "tens tens x,y,z"
-- success; tens is associative

tens5 = parseTester tensParse "tens \\(x:I).y,z"
-- success

tens6 = parseTester tensParse "tens x , \\(y:I).y"

------------------------------------------------------------------------
-- Let Unit Testing                         			      --
------------------------------------------------------------------------
letu0 = parseTester letUParse "let unit = unit in unit"
-- success

letu1 = parseTester letUParse "let unit = x in tens y,z"
-- success

letu2 = parseTester letUParse "let unit = (a b) in \\(x:I -o I).y"
-- success

------------------------------------------------------------------------
-- Let Tensor Testing			      --
------------------------------------------------------------------------

lett0 = parseTester letTParse "let x(x)y = unit in unit"
lett1 = parseTester letTParse "let a(x)b = \\(x:I).y z in zz"

lett2 = parseTerm "let x(x)y = \\(l:I).m n in \\(o:I).p q"
lett3 = parseTester letTParse "let x(x)y = \\(l:I).m n in \\(o:I).p q"
------------------------------------------------------------------------
-- Main testing functions                        		      --
------------------------------------------------------------------------

lammain = do
   putStrLn $ "lamtest0: " ++ (runPrettyTerm $ lamtest0)
   putStrLn $ "lamtest1: " ++ (runPrettyTerm $ lamtest1)
   -- putStrLn $ "lamtest2: " ++ (runPrettyTerm $ lamtest2)
   putStrLn $ "lamtest3: " ++ (runPrettyTerm $ lamtest3)
   putStrLn $ "lamtest4: " ++ (runPrettyTerm $ lamtest4)
   putStrLn $ "lamtest5: " ++ (runPrettyTerm $ lamtest5)
   putStrLn $ "lamtest6: " ++ (runPrettyTerm $ lamtest6)
   putStrLn $ "lamtest7: " ++ (runPrettyTerm $ lamtest7)
   putStrLn $ "lamtest8: " ++ (runPrettyTerm $ lamtest8)
   putStrLn $ "lamtest9: " ++ (runPrettyTerm $ lamtest9)
   putStrLn $ "lamtest10: " ++ (runPrettyTerm $ lamtest10)

tensmain = do
   putStrLn $ "tens0: " ++ (runPrettyTerm $ tens0)
   putStrLn $ "tens1: " ++ (runPrettyTerm $ tens1)
   putStrLn $ "tens2: " ++ (runPrettyTerm $ tens2)
   putStrLn $ "tens3: " ++ (runPrettyTerm $ tens3)
   putStrLn $ "tens4: " ++ (runPrettyTerm $ tens4)
   putStrLn $ "tens5: " ++ (runPrettyTerm $ tens5)
   putStrLn $ "tens6: " ++ (runPrettyTerm $ tens6)

letUmain = do
   putStrLn $ "letu0: " ++ (runPrettyTerm $ letu0)
   putStrLn $ "letu1: " ++ (runPrettyTerm $ letu1)
   putStrLn $ "letu2: " ++ (runPrettyTerm $ letu2)

letTmain = do
   putStrLn $ "lett0: " ++ (runPrettyTerm $ lett0)
   putStrLn $ "lett1: " ++ (runPrettyTerm $ lett1)
   putStrLn $ "lett2: " ++ (runPrettyTerm $ lett2)
   putStrLn $ "lett3: " ++ (runPrettyTerm $ lett3)

main = do
   lammain
   tensmain
   letUmain
   letTmain







