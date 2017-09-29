module F5LAE where 

type Name = String  
type FormalArg = String 
type Id = String 
type DefrdSub = [(Id, Value)]

data FunDec = FunDec Name [FormalArg] Exp 
 deriving(Read, Show, Eq)

data Value = NumValue Integer
        | Closure FormalArg Exp  
 deriving(Read, Show, Eq)

data Exp = Num Integer
         | Add Exp Exp 
         | Sub Exp Exp 
         | Let Id Exp Exp 
         | Ref Id
         | App Name [Exp] 
         | Lambda FormalArg Exp
         | LambdaApp Exp Exp 
         | IfZero Exp Exp Exp
 deriving(Read, Show, Eq)

-- INTERPRETE

interp :: Exp -> DefrdSub -> [FunDec] -> Value

interp (Num n) _ _             = NumValue n
interp (Add l r) subs decs     = binOperation (+) l r subs decs 
interp (Sub l r) subs decs     = binOperation (-) l r subs decs

interp (Let x e1 e2) subs decs = interp e2 ((x,exp):subs) decs 
  where
    exp = interp e1 subs decs

interp (Ref v) subs _ =
  let r = lookupV v subs
  in case r of
      (Nothing) -> error "Variable not declared" -- Caso não ache a variavel na lista 
      (Just v ) -> v                                   

interp (Lambda form_arg expression) subs decs  = Closure form_arg expression

interp (LambdaApp e1 e2) subs decs = interp e ((form_arg, expression) : subs) decs -- Terminada ( falta testar ) decs_closure= lista closure
  where
    expression = interp e2 subs decs
    (Closure form_arg e) = interp e1 subs decs

interp (App n expression) subs decs = interp expf subs2 decs
  where
    (FunDec name form_arg expf) = let f = lookupF n decs
                              in case f of
                                  (Nothing) -> error "Function not declared"
                                  (Just fundec) -> fundec
    subs2 = listFunDec form_arg expression subs decs 

interp (IfZero cond true false) subs decs
  | (interp cond subs decs) == (NumValue 0) = interp true subs decs
  | otherwise = interp false subs decs

-- ESTRUCTURES

binOperation :: (Integer -> Integer -> Integer) -> Exp -> Exp -> DefrdSub -> [FunDec] -> Value 
binOperation op e1 e2 subs decs = interp (Num (op n1 n2)) subs decs  
 where 
  (NumValue n1) = interp e1 subs decs
  (NumValue n2) = interp e2 subs decs

lookupF :: Name -> [FunDec] -> Maybe FunDec
lookupF _ [] = Nothing 
lookupF name (fun@(FunDec n form_arg expression):fun_s) --fun_s lista funcoes [FunDec n form_arg expression]
  | name == n = Just fun
  | otherwise = lookupF name fun_s 


lookupV :: Name -> DefrdSub -> Maybe Value -- Retornar somente o valor
lookupV _ [] = Nothing 
lookupV name ((id, value):cs) 
  | name == id = Just value
  | otherwise = lookup name cs


listFunDec :: [FormalArg] -> [Exp] -> DefrdSub -> [FunDec] -> DefrdSub
listFunDec [] [] subs decs = subs
listFunDec _  [] subs decs = error "Variables not declared"
listFunDec [] _ subs decs  = error "Variables not declared"
listFunDec (arg:args) (exp:exps) subs decs = listFunDec args exps ((arg , eval) : subs) decs
  where
    eval = interp exp subs decs

let2lambda :: Exp -> Exp
let2lambda (Let x e1 e2) = LambdaApp (Lambda x e2) e1
let2lambda _ = error "Wrong use of let2lambda"


