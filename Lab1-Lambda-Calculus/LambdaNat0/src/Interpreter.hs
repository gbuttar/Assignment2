import AbsLambdaNat -- defines "Program", "Exp", "EApp", "EAbs", etc
import ErrM
import PrintLambdaNat

import Data.Map ( Map )
import qualified Data.Map as M

-- execCBN is a function from type Program to type Exp, calling evalCBN
-- CBN is short for Call By Name, see the lectures
-- the types Program and Exp are defined in AbsLambdaNat.hs
execCBN :: Program -> Exp
execCBN (Prog e) = evalCBN e
-- evalCBN is the actual interpreter ("eval" for evaluate, "CBN" for call by name)
evalCBN :: Exp -> Exp

evalCBN (app e1 e2) = case (evalCBN e1) of
    (eAbs i e3 )-> evalCBN (subst i e2 e3)
    e3 -> eApp e3 e2

evalCBN eNat0 = eNat0
evalCBN (eNatS e1) = eNatS (evalCBN e1)
----------------------------------------------------
--- YOUR CODE goes here for extending the interpreter
----------------------------------------------------
evalCBN x = x -- this is a catch all clause, currently only for variables, must be the clause of the eval function

-- fresh generates fresh names for substitutions, can be ignored for now
-- a quick and dirty way of getting fresh names. Rather inefficient for big terms...
fresh_aux :: Exp -> String
fresh_aux (eVar (Id i)) = i ++ "0"
fresh_aux (eApp e1 e2) = fresh_aux e1 ++ fresh_aux e2
fresh_aux (eAbs (Id i) e) = i ++ fresh_aux e
--fresh_aux _ = "0"

fresh = Id . fresh_aux -- for Id see AbsLamdaNat.hs

-- subst implements the beta rule
-- (\x.e)e' reduces to subst x e' e
subst :: Id -> Exp -> Exp -> Exp
subst id s (eVar id1) | id == id1 = s
                      | otherwise = eVar id1
subst id s (eApp e1 e2) = eApp (subst id s e1) (subst id s e2)
subst id s (eAbs id1 e1) =
    -- to avoid variable capture, we first substitute id1 with a fresh name inside the body
    -- of the λ-abstraction, obtaining e2.
    -- Only then do we proceed to apply substitution of the original s for id in the body e2.
    let f = fresh (eAbs id1 e1)
        e2 = subst id1 (eVar f) e1 in
        eAbs f (subst id s e2)
----------------------------------------------------------------
subst id s eNat0 = eNat0
subst id s (eNatS e1) = eNÍatS (subst id s e1)
----------------------------------------------------------------
