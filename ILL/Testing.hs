module Testing where

import Syntax
import Parser
import Pretty

lamtest0 = parseTester lamParse "\\(x:I).y"
-- successful

lamtest1 = parseTester lamParse "\\(x:I(x)I).y"
-- successful

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

lamtest8 = parseTester lamParse "\\(x:(I -o I)(x)(I -o I)).y unit (x) unit"
-- unsuccessful; bad parse

lamtest8' = parseTester lamParse "\\(x:(I -o I)(x)(I -o I)).y unit(x)unit"

lamtest9 = parseTester lamParse "\\(x:(I -o I)(x)(I -o I)).y (unit (x) unit)"
-- unsuccesful; tries to parse App Unit (var x)

lamtest10 = parseTester lamParse "\\(x:(I -o I)(x)(I -o I)).y (unit(x)unit)"

main = do
  putStrLn $ "lamtest0: " ++ (runPrettyTerm $ lamtest0)
  putStrLn $ "lamtest1: " ++ (runPrettyTerm $ lamtest1)
  -- putStrLn $ "lamtest2: " ++ (runPrettyTerm $ lamtest2)
  putStrLn $ "lamtest3: " ++ (runPrettyTerm $ lamtest3)
  putStrLn $ "lamtest4: " ++ (runPrettyTerm $ lamtest4)
  putStrLn $ "lamtest5: " ++ (runPrettyTerm $ lamtest5)
  putStrLn $ "lamtest6: " ++ (runPrettyTerm $ lamtest6)
  putStrLn $ "lamtest7: " ++ (runPrettyTerm $ lamtest7)
  putStrLn $ "lamtest8: " ++ (runPrettyTerm $ lamtest8) -- *need to eval this
  putStrLn $ "lamtest9: " ++ (runPrettyTerm $ lamtest9)
  putStrLn $ "lamtest10: " ++ (runPrettyTerm $ lamtest10)
