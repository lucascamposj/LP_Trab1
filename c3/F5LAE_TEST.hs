module F5LAE_TEST where 

import Test.HUnit 
import F5LAE


parse :: String -> Exp
parse s = read s 

-- Testes da questão 2

-- exp1, exp2 :: String 

exp13 = Let ("x") (Num 3) (Let ("f") (Lambda ("y") (Add (Ref "y") (Ref "x"))) (Let ("x") (Num 5) (LambdaApp (Ref "f") (Num 4))))

tc13  = TestCase (assertEqual "tc13" (interp (exp13) [] []) (NumValue 9))
