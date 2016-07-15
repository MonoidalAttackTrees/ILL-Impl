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

lamtest9 = parseTester lamParse "\\(x:(I -o I)(x)(I -o I)).y (unit (x) unit)"
-- unsuccesful; tries to parse App Unit (var x)

lamtest10 = parseTester lamParse "\\(x:(I -o I)(x)(I -o I)).y (unit(x)unit)"

main = do
 putStrLn $ runPrettyTerm $ lamtest0
 putStrLn $ runPrettyTerm $ lamtest1
 -- putStrLn $ runPrettyTerm $ lamtest2
 putStrLn $ runPrettyTerm $ lamtest3
 putStrLn $ runPrettyTerm $ lamtest4
 putStrLn $ runPrettyTerm $ lamtest5
 putStrLn $ runPrettyTerm $ lamtest6
 putStrLn $ runPrettyTerm $ lamtest7 
 putStrLn $ runPrettyTerm $ lamtest8
 putStrLn $ runPrettyTerm $ lamtest9
 putStrLn $ runPrettyTerm $ lamtest10
