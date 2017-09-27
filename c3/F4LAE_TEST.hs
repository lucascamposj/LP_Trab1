module F4LAE_TEST where 

import Test.HUnit 
import F4LAE


parse :: String -> Exp
parse s = read s 


exp11 = "IfZero (Num 0) (Num 3) (Num 4)"
exp12 = "App \"test12\" [(Num 2), (Num 3)]"





tc11  = TestCase (assertEqual "tc11" (interp (parse exp11) [] []) (NumValue 3))

tc12  = TestCase (assertEqual "tc12" (interp (parse exp12) [] [(FunDec "test12" ["x","y"] (Add (Ref "x") (Ref "y")))]) (NumValue 5))

tests = TestList [ TestLabel "tc11" tc11, 
                    TestLabel "tc12" tc12
                 ]