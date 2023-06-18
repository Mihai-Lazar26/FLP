import Data.List

type Variable = Char 
data Term = V Variable
    | App Term Term 
    | Lam Variable Term 
    deriving (Eq, Show)
--Cerinta1
lambda1 = Lam 'x' (App (V 'x') (App(V 'y')(V 'z')))
lambda2 = App (Lam 'x' (App (V 'x') (V 'y'))) (App (V 'y') (Lam 's' (App (V 's') (V 'z'))))
lambda3 = Lam 's' (Lam 'z' (App (V 's')(App (V 's')(V 'z'))))

--Cerinta2
remove :: Eq a => [a] -> a -> [a]
remove [] _ = []
remove l val = [x | x <- l, x /= val]


freeVars :: Term -> [Variable]
freeVars (V x) = [x]
freeVars (App t1 t2) = nub $ (freeVars t1) ++ (freeVars t2)
freeVars (Lam x t) = nub $ remove (freeVars t) x

--Cerinta3

isMember :: Eq a => a -> [a] -> Bool
isMember _ [] = False
isMember y (x:xs) =
 if y == x then True else isMember y xs

subst :: Term -> Variable -> Term -> Term
subst u x (V y)
    | x == y = u
    | otherwise = V y
subst u x (App t1 t2) = App (subst u x t1) (subst u x t2)
subst u x (Lam y t) 
    | x /= y && (isMember y (freeVars u)) == False = Lam y (subst u x t)
    | otherwise = error "Nu se poate face substitutia"