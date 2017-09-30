module F4LAE_TEST where 

import Test.HUnit 
import F4LAE


parse :: String -> Exp
parse s = read s 

-- Testes da questão 2 e 3

-- exp1, exp2 :: String 

exp1 = IfZero (Num 0) (Num 3) (Num 4)

exp2 = App "test2" [(Num 2), (Num 3)]

exp3 = Let ("x") (Num 0) (IfZero (Ref "x") (Num 5) (Num 10))

exp4 = Let ("y") (Num 5) (Let ("z") (Num 5) (IfZero (Sub (Ref "z") (Ref "y")) (Num 5) (Num 10)))

exp5 = App "test5" [(Num 2), (Num 3), (Num 4), (Num 5)]

tc1  = TestCase (assertEqual "tc1" (interp (exp1) [] []) (NumValue 3))

tc2  = TestCase (assertEqual "tc2" (interp (exp2) [] [(FunDec "test2" ["x","y"] (Add (Ref "x") (Ref "y")))]) (NumValue 5))

tc3  = TestCase (assertEqual "tc3" (interp (exp3) [] []) (NumValue 5))

tc4  = TestCase (assertEqual "tc4" (interp (exp4)[] []) (NumValue 5))

tc5  = TestCase (assertEqual "tc5" (interp (exp5) [] [(FunDec "test5" ["x","y","z","p"] (Add (Ref "x") (Add (Ref "y") (Add (Ref "z") (Ref "p")))))]) (NumValue 14))

tests = TestList [ TestLabel "tc1" tc1, 
                    TestLabel "tc2" tc2,
                    TestLabel "tc3" tc3, 
                    TestLabel "tc4" tc4,
                    TestLabel "tc5" tc5
                 ]