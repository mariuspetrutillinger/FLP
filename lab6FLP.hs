-- Evaluarea expresiilor
-- newtype Var = Var {getVar :: String}

-- data ComplexExp =
--     | CX Var
--     | Nat Natural
--     | CLam Var ComplexExp
--     | CApp ComplexExp ComplexExp
--     | Let Var ComplexExp ComplexExp
--     | LetRec Var ComplexExp ComplexExp
--     | List [ComplexExp]

-- let x = exp1 in exp2
-- (lambda x.exp2) exp1
-- let x = 2 in x + x
-- (lambda x. x + x ) 2

-- Singurele operatii care sunt necesarea pt a reprezenta tot limbajul sunt operatiile din lambda-calcul
-- Cum se defineste un lambda termen? 
--  t::= x |t t | lambda.x t
--      var |aplicare| abstractizare

-- Ce este ComplexExp pt lambda-expresii?
-- Sugar sintax pentru lambda-calcul

-- Pentru a evalua expresiile complexe (ComplexExp), e suficient sa le reduc la o lambda expresie si sa o evaluez
-- - am de evaluat doar 3 cazuri in loc de 7\
-- - dintre cele 7, 4 ar fi fost oricum redundante

-- Introducem un tip de date Exp pentru reprezentarea lambda-expresiilor
-- Vom introduce un alt tip de date pentru variabile

-- newtype IndexVar = IndexVar {ivName ::String, ivCount :: Int}

-- unicitatea unei variabile este asigurata de ivCount
-- daca vreau sa generez un nou "x" ma uit la toate aparitiile lui in sist si la countul fiecarei aparitii
-- si generez un nou x cu countul maxim + 1

-- data Exp =
--     | Var IndexVar
--     | App Exp Exp
--     | Lam IndexVar Exp

-- cele 2 tipuri ComplexExp si Exp sunt echival

-- deSugar :: ComplexExp -> Exp

-- sugar :: Exp -> ComplexExp

-- Evaluarea lambda expresiilor

-- Pentru a evalua o lambda expresie, avem nevoie sa implementam notiunea de substitutie. Pentru substitutie,
-- am nevoie de fct auxiliare

-- vars :: Exp -> [IndexVar]

-- t :: x | t t | lambda.x t

-- vars (x) = [x]
-- vars (t1 t2) = vars t1 u vars t2
-- vars (lambda.x t) = vars t u {x}

-- implementati ac functie in haskell
-- pentru reuniune aveti union in Data.List

import Data.List

newtype Var = Var {getVar :: String} 

data ComplexExp =
  | CX Var
  | Nat Natural
  | CLam Var ComplexExp
  | CApp ComplexExp ComplexExp
  | Let Var ComplexExp ComplexExp
  | LetRec Var ComplexExp ComplexExp
  | List [ComplexExp]

data Exp =
  | Var IndexVar
  | App Exp Exp
  | Lam IndexVar Exp

vars :: Exp -> [IndexVar]

vars (X x) = [x]
vars (App t1 t2) = vars t1 `union` vars t2
vars (Lam x t) = vars t `union` [x]

Definim functia 
freeVars :: Exp -> [IndexVar]

-- x y lambda x. x y z ; 

-- FV (x) = {x}
-- FV (t1 t2) = FV t1 u FV t2
-- FV (lambda.x t) = FV t - {x}

-- impl in hs

freeVars (X x) = [x]
freeVars (App t1 t2) = freeVars t1 `union` freeVars t2
freeVars (Lam x t) = delete x $ freeVars t

occursFree :: IndexVar -> Exp -> Bool
occursFree x e = elem x $ freeVars e

freshVar :: IndexVar -> [IndexVar] -> IndexVar
freshVar x xs = x {ivCount = m + 1}
    where
        nxs = [ivCount y | y <- xs, ivName x = ivName y]
        n = maximum nxs

renameVar :: IndexVar -> IndexVar -> Exp -> Exp
-- x si vreau sa l inlocuiesc pe x cu u -> u
-- x si vreau sa l inlocuiesc pe y cu z -> x
renameVar toReplace replacement = go
    where
        go (X x) = (if x == toReplace) then  (X replacement) else  (X x)
        go (App t1 t2) = App (go t1) (go t2)
        go (Lam x t) = Lam (if x == toReplace then replacement else x)  (go t)

substitutie :: IndexVar -> Exp -> Exp -> Exp
substitutia toReplace replacement = go
     where
        go (X x) = (if x == toReplace) then replacement else (X x)
        go (App t1 t2) = App (go t1) (go t2)
        go (Lam x t) 
         | x == toReplace = Lam x t
         | occursFree x replacement = 
             let f = freshVar x (vars t `union` vars replacement)
             -- daca var de legatura apare libera unde facem legatura
              in Lam f (go (renameVar x f t))
         | otherwise = Lam x (go t)


-- sol : redenumim x ul din legatura

-- [x lambda r. ry /z] lambda x. zt = lambda x. (x lambda r. ry) t
-- in general, cand vreau sa inlocuiesc variabila de legatura, nu fac nicio alta modificare

-- pentru lab urmator
-- deSugar :: ComplexExp -> Exp
-- sugar :: Exp -> ComplexExp  <- simpla

sugar :: Exp -> ComplexExp
sugar (X x) = CX (Var (ivName x))
sugar (App t1 t2) = CApp (sugar t1) (sugar t2)
--sugar (Lam x t) = CLam (Var (ivName x)) (sugar t) DE MODIFICAT








