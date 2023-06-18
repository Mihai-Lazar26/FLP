import Data.List
import Data.Maybe

type Name = String

data Value = VBool Bool
    | VInt Int
    | VFun (Value -> Value)
    | VError

data Hask = HTrue --
    | HFalse --
    | HIf Hask Hask Hask --
    | HLit Int --
    | HLet Name Hask Hask -- 
    | Hask :==: Hask --
    | Hask :+:  Hask -- 
    | Hask :*: Hask -- tema 
    | HVar Name -- 
    | HLam Name Hask --
    | Hask :$: Hask
    deriving (Show)

infix 4 :==:
infixl 6 :+:
--infixl 6 :-:
infixl 7 :*: 
infixl 9 :$:

type HEnv = [(Name, Value)]

showV :: Value -> String
showV (VBool b) = show b
showV (VInt i) = show i
showV (VFun _) = "<function>"
showV (VError) = "<error>"

instance Show Value where 
    show = showV 

eqV :: Value -> Value -> Bool
eqV (VBool b) (VBool c) =  b == c
eqV (VInt i) (VInt j) =  i == j
eqV _ _ = False

instance Eq Value where 
    (==) = eqV 

hEval :: Hask -> HEnv -> Value
hEval HTrue _ = VBool True
hEval HFalse _ = VBool False 
hEval (HLit x) _ = VInt x 
hEval (HIf cond sttrue stfalse) env = hif (hEval cond env) (hEval sttrue env) (hEval stfalse env)
    where 
        hif (VBool b) st1 st2 = if b then st1 else st2 
        hif _ _ _ = error "Eroare HIf" 
hEval (HLet x valexpr expr) env = hEval expr ((x, hEval valexpr env) : env)
hEval (exp1 :==: exp2) env = VBool $ (hEval exp1 env) == (hEval exp2 env)
hEval (exp1 :+: exp2) env = hadd (hEval exp1 env) (hEval exp2 env)
    where 
        hadd (VInt i) (VInt j) = VInt $ i + j 
        hadd _ _ = error "Eroare :+:" 
hEval (HVar x) env = fromMaybe VError $ lookup x env 
hEval (HLam x expr) env = VFun (\v -> hEval expr ((x, v) : env))
hEval (f :$: g) env = happ (hEval f env) (hEval g env)
    where 
        happ (VFun f) v = f v 
        happ _ _ = error "Eroare :$:" 
hEval (exp1 :*: exp2) env = hProd (hEval exp1 env) (hEval exp2 env)
    where
        hProd (VInt i) (VInt j) = VInt $ i * j
        hProd _ _ = error "Eroare :*:"

run :: Hask -> String
run pg = showV (hEval pg [])

h0 =  (HLam "x" (HLam "y" ((HVar "x") :+: (HVar "y")))) :$: (HLit 3) :$: (HLit 4)
h0Haskell = (\x -> (\y -> x + y) $ 3) $ 4

h1 = ((HLam "x" ((HVar "x") :*: (HLit 2))) :$: (HLit 4)) :==: HLit 8
h2 = HIf ((HLam "x" (HLam "y" ((HVar "x") :==: (HVar "y")))) :$: (HLit 2) :$: (HLit 1)) (HLit 100) (HLit 200) 
h3 = HIf (HLam "x" (HVar "x" :==: HTrue) :$: HFalse) (HFalse) (HTrue) 
h4 = (HLam "x" (HLam "y" (((HVar "x") :+: (HVar "y")) :==: ((HVar "x") :*: (HLit 2))))) :$: (HLit 5) :$: (HLit 5)

h5 = HFalse :+: HLit 2 --Eroare

--hEvalMaybe :: Hask -> HEnv -> Maybe Value
--hEvalMaybe HTrue _ = Just (VBool True)
--hEvalMaybe HFalse _ = Just (VBool False) 
--hEvalMaybe (HLit x) _ = Just (VInt x) 
--hEvalMaybe (HIf cond sttrue stfalse) env = hif (hEvalMaybe cond env) (hEvalMaybe sttrue env) (hEvalMaybe stfalse env)
--    where 
--        hif (Just(VBool b)) st1 st2 = if b then st1 else st2 
--        hif _ _ _ = Nothing
--hEvalMaybe (HLet x valexpr expr) env = hEvalMaybe expr ((x, hEvalMaybe valexpr env) : env)
--hEvalMaybe (exp1 :==: exp2) env = Just (VBool $ (hEvalMaybe exp1 env) == (hEvalMaybe exp2 env))
--hEvalMaybe (exp1 :+: exp2) env = hadd (hEvalMaybe exp1 env) (hEvalMaybe exp2 env)
--    where 
--        hadd (Just (VInt i)) (Just (VInt j)) = Just (VInt $ i + j) 
--        hadd _ _ = Nothing 
--hEvalMaybe (HVar x) env = fromMaybe (Just VError) $ lookup x env 
--hEvalMaybe (HLam x expr) env = Just (VFun (\v -> hEvalMaybe expr ((x, v) : env)))
--hEvalMaybe (f :$: g) env = happ (hEvalMaybe f env) (hEvalMaybe g env)
--    where 
--        happ (Just (VFun f)) v = Just (f v) 
--        happ _ _ = Nothing 
--hEvalMaybe (exp1 :*: exp2) env = hProd (hEvalMaybe exp1 env) (hEvalMaybe exp2 env)
--    where
--        hProd (Just (VInt i)) (Just (VInt j)) = Just (VInt $ i * j)
--        hProd _ _ = Nothing

-- Tema (pana maine seara - 14.05.2022 23:59)
-- 1. adaugati semantica denotationala pentru :*: 
-- 2. evaluati programul h0 si scrieti echivalentul lui in Haskell 
-- 3. adaugati inca 4 exemple de expresii Hask si evaluati-le. Expresiile trebuie sa fie complexe si sa utilizeze toate definitiile 
-- 4. modificati, pentru fiecare caz in parte, return-ul VError intr-un mesaj de eroare specific, utilizand functia error din Haskell 
-- 5. modificati signatura functiei hEval in hEval :: Hask -> HEnv -> Maybe Value. Returneaza Just cand se poate evalua, si Nothing in caz de eroare.


-- laboratorul se noteaza pentru punctajul suplimentar 