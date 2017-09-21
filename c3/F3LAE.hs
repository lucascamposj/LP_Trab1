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
	exp    = interp e1 subs decs
	
interp (Ref v) subs _			= 
  let r = lookupC v subs
  in case r of
      (Nothing) -> error "Variable not declared" -- Caso não ache a variavel na liste 
      (Just f ) -> f                                    --

interp (Lambda v b) subs decs  = Closure v exp subs

-- interp (LambdaApp e1 e2) subs decs = interp e2 subs decs
--   where 
-- 	(Lambda x e) = interp e1 subs decs
-- 	sub          = (subst x e2 e decs)  
interp (LambdaApp e1 e2) subs decs = interp e ((foa,w) : decsclo) -- Terminada ( falta testar ) 
  where
    w = interp e2 subs decs
    (Closure foa e decsclo) = interp e1 subs decs

interp (App n e) subs decs =
  let f = lookupF n decs
  in case f of
      (Nothing) -> error "Function not declared"
      (Just (FunDec m a b)) -> interp (subst a e b decs) decs  

-- ESTRUCTURES

binOperation :: (Integer -> Integer -> Integer) -> Exp -> Exp -> DefrdSub -> [FunDec] -> Value 
binOperation op e1 e2 subs decs = interp Num (op n1 n2)  
 where 
  (Num n1) = interp e1 subs decs
  (Num n2) = interp e2 subs decs

lookupF :: Name -> [FunDec] -> Maybe FunDec
lookupF _ [] = Nothing 
lookupF name (fun@(FunDec n a b):fs)
  | name == n = Just fun
  | otherwise = lookupF name fs 

lookupC :: Name -> DefrdSub -> Maybe Value -- Retornar somente o valor
lookupC _ [] = Nothing 
lookupC name (fun@(Closure func x s):cs)
  | name == func = Just fun
  | otherwise = lookup name cs
lookupC name (NumValue value:cs)
  | name == 



