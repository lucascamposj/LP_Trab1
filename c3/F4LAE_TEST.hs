module F4LAE_TEST where 

import Test.HUnit 
import F4LAE


parse :: String -> Exp
parse s = read s 

-- Testes da questão 2

-- exp1, exp2 :: String 

exp11 = IfZero (Num 0) (Num 3) (Num 4)
exp12 = App "test12" [(Num 2), (Num 3)]





tc11  = TestCase (assertEqual "tc11" (interp (exp11) [] []) (NumValue 3))

tc12  = TestCase (assertEqual "tc12" (interp (exp12) [] [(FunDec "test12" ["x","y"] (Add (Ref "x") (Ref "y")))]) (NumValue 5))

