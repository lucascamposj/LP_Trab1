module F5LAE_TEST where 

import Test.HUnit 
import F5LAE


parse :: String -> Exp
parse s = read s 


exp13 = "Let (\"x\") (Num 3) (Let (\"f\") (Lambda (\"y\") (Add (Ref \"y\") (Ref \"x\"))) (Let (\"x\") (Num 5) (LambdaApp (Ref \"f\") (Num 4))))"

-- exp14 = "Let (\"x\") (Num 4) (Let (\"f\") (Lambda (\"z\") (Sub (Ref \"x\") (Ref \"z\"))) (Let (\"z\") (Num 5) (LambdaApp (Ref \"f\") (Num 5))))"

tc13  = TestCase (assertEqual "tc13" (interp (parse exp13) [] []) (NumValue 9))

-- tc14  = TestCase (assertEqual "tc14" (interp (parse exp14) [] []) (NumValue 10))

tests = TestList [ TestLabel "tc13" tc13]