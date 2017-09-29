module F3LAE_TEST where 

import Test.HUnit 
import F3LAE


parse :: String -> Exp
parse s = read s 

-- Testes da questão 1

-- exp1, exp2 :: String 
exp1 = Num 8
exp2 = Add (Num 3) (Num 4)
exp3 = Sub (Num 3) (Num 5)
exp4 = Ref "x"
exp5 = App "funcao" (Num 4) 
exp6 = Let ("x") (Num 3) (Num 4)
exp7 = Let ("y") (Num 4) (Add (Ref "y") (Num 4))
exp8 = Let ("x") (Num 3) (Let ("f") (Lambda ("y") (Add (Ref "y") (Ref "x"))) (Let ("x") (Num 5) (LambdaApp (Ref "f") (Num 4))))
exp9 = Lambda ("x") (Add (Ref "x") (Num 1))
exp10 = Ref "x"

tc1  = TestCase (assertEqual "tc01" (interp (exp1) [] []) (NumValue 8))

tc2  = TestCase (assertEqual "tc02" (interp (exp2) [] []) (NumValue 7))

tc3  = TestCase (assertEqual "tc03" (interp (exp3) [] []) (NumValue (-2)))

tc4  = TestCase (assertEqual "tc04" (interp (exp4) [("x",NumValue 3)] []) (NumValue 3))

tc5  = TestCase (assertEqual "tc05" (interp (exp5) [] [(FunDec "funcao" "x" (Add (Ref "x") (Num 4)))]) (NumValue 8))

tc6  = TestCase (assertEqual "tc06" (interp (exp6) [] []) (NumValue 4))

tc7  = TestCase (assertEqual "tc07" (interp (exp7) [] []) (NumValue 8))

tc8  = TestCase (assertEqual "tc08" (interp (exp8) [] []) (NumValue 7))

tc9  = TestCase (assertEqual "tc09" (interp (exp9) [] []) (Closure "x" (Add (Ref "x") (Num 1)) []))

tc10 = TestCase (assertString  "tc10" (interp (exp10) [] []) ("Variable not declared"))



