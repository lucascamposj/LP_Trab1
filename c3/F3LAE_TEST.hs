module F3LAE_TEST where 

import Test.HUnit 
import F3LAE


parse :: String -> Exp
parse s = read s 

-- some HUnit test cases to better understand the calc semantics

exp1, exp2 :: String 
exp1 = "Num 8"
exp2 = "(Let (Ref x) (Num 3) (Let (Ref f) (Lambda (Ref y) ((Ref y) + (Ref x)) (Let (Ref x) (Num 5) (LambdaApp (Ref f) (Num 4) ))) [] [])" 

tc1 = TestCase (assertEqual "tc01" (interp (parse exp1)) (Num 8))

tc2 = TestCase (assertEqual "tc02" (interp (parse exp2)) (Num 8)) 

