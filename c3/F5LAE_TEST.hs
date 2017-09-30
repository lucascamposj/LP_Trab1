module F5LAE_TEST where 

import Test.HUnit 
import F5LAE


parse :: String -> Exp
parse s = read s 

-- Testes da questão 4


exp1 = Let ("x") (Num 3) (Let ("f") (Lambda ("y") (Add (Ref "y") (Ref "x"))) (Let ("x") (Num 5) (LambdaApp (Ref "f") (Num 4))))

exp2 = Let ("x") (Num 4) (Let ("f") (Lambda ("z") (Sub (Ref "x") (Ref "z"))) (Let ("z") (Num 5) (LambdaApp (Ref "f") (Num 5))))

tc1  = TestCase (assertEqual "tc1" (interp (exp13) [] []) (NumValue 9))

tc2  = TestCase (assertEqual "tc2" (interp (exp14) [] []) (NumValue 10))

tests = TestList [ TestLabel "tc1" tc1, 
                    TestLabel "tc2" tc2
                 ]