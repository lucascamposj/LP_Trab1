module F5LAE_TEST where 

import Test.HUnit 
import F5LAE


parse :: String -> Exp
parse s = read s 

-- Testes da questão 4


exp1 = Let ("x") (Num 3) (Let ("f") (Lambda ("y") (Add (Ref "y") (Ref "x"))) (Let ("x") (Num 5) (LambdaApp (Ref "f") (Num 4))))

exp2 = Let ("x") (Num 4) (Let ("f") (Lambda ("z") (Sub (Ref "x") (Ref "z"))) (Let ("z") (Num 5) (LambdaApp (Ref "f") (Num 5))))

exp3 = App "f" [(Ref "x"), (Ref "y")]

exp4 = Let ("x") (Num 3) (Let ("f") (Lambda ("z")  (Add (Ref "z") (Ref "x"))  ) ((Let ("x") (Num 5) (Let ("w") (LambdaApp (Ref "f") (Num 4)) (Add (Ref "w") (Ref "x"))))))

tc1  = TestCase (assertEqual "tc1" (interp (exp1) [] [])     (NumValue 9))

tc2  = TestCase (assertEqual "tc2" (interp (exp2) [] [])     (NumValue (-1)))

tc3 = TestCase (assertEqual "tc3" (interp (exp3) [] [FunDec "f" ["x","y"] (Let "x" (Num 20) (exp1)) ]) (NumValue 9))

tc4 = TestCase (assertEqual "tc4" (interp (exp4) [] [])     (NumValue 14))

tests = TestList [ TestLabel "tc1" tc1, 
                    TestLabel "tc2" tc2,
                    TestLabel "tc3" tc3,
                    TestLabel "tc4" tc4
                 ]