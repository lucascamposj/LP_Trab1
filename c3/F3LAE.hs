module F3LAE where 

type Name = String  
type FormalArg = String 
type Id = String 
type DefrdSub = [(Id, Value)] 

data FunDec = FunDec Name FormalArg Exp 
 deriving(Read, Show, Eq)


data Value = NumValue Integer
        | Closure FormalArg Exp DefrdSub  
 deriving(Read, Show, Eq)

data Exp = Num Integer
         | Add Exp Exp 
         | Sub Exp Exp 
         | Let Id Exp Exp 
         | Ref Id
         | App Name Exp 
         | Lambda FormalArg Exp
         | LambdaApp Exp Exp 
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
      (Nothing) -> error "Variable not declared" -- Caso não ache a variavel na liste 
      (Just v ) -> v                                    --

interp (Lambda v b) subs decs  = Closure v b subs

interp (LambdaApp e1 e2) subs decs = interp e ((foa,w) : decsclo) decs -- Terminada ( falta testar ) 
  where
    w = interp e2 subs decs
    (Closure foa e decsclo) = interp e1 subs decs

interp (App n e) subs decs = interp expf subs2 decs
  where
    (FunDec name fa expf) = let f = lookupF n decs
                              in case f of
                                  (Nothing) -> error "Function not declared"
                                  (Just fundec) -> fundec
    l1 = interp e subs decs
    subs2 = ((fa,l1):subs)


-- ESTRUCTURES

binOperation :: (Integer -> Integer -> Integer) -> Exp -> Exp -> DefrdSub -> [FunDec] -> Value 
binOperation op e1 e2 subs decs = interp (Num (op n1 n2)) subs decs  
 where 
  (NumValue n1) = interp e1 subs decs
  (NumValue n2) = interp e2 subs decs

lookupF :: Name -> [FunDec] -> Maybe FunDec
lookupF _ [] = Nothing 
lookupF name (fun@(FunDec n a b):fs)
  | name == n = Just fun
  | otherwise = lookupF name fs 


lookupV :: Name -> DefrdSub -> Maybe Value -- Retornar somente o valor
lookupV _ [] = Nothing 
lookupV name ((id, value):cs)
  | name == id = Just value
  | otherwise = lookup name cs




