module Interpreter ( execCBN ) where

import AbsLambdaNat
import ErrM
import PrintLambdaNat

import Data.Map ( Map )
import qualified Data.Map as M
execCBN :: Program -> Exp
execCBN (Prog e) = evalCBN e

evalCBN :: Exp -> Exp

evalCBN (EApp e1 e2) = case (evalCBN e1) of
    (EAbs i e3) -> evalCBN (subst i e2 e3)
    e3 -> EApp e3 e2

evalCBN ENat0 = ENat0

evalCBN (ENatS e) = ENatS (evalCBN e)

evalCBN (EIf e1 e2 e3 e4) =
  if (evalCBN e1) == (evalCBN e2)
    then (evalCBN e3)
    else (evalCBN e4)

evalCBN (ELet i e1 e2) =
  evalCBN (EApp (EAbs i e2) e1)

evalCBN (ERec i e1 e2) = evalCBN (EApp (EAbs i e2) (EFix (EAbs i e1)))

evalCBN (EFix e) = evalCBN (EApp e (EFix e))

evalCBN ENil = ENil

evalCBN (ECons e1 e2) = ECons (evalCBN e1) (evalCBN e2)

evalCBN (ELE e1 e2) = ECons (evalCBN e1) (evalCBN e2)

evalCBN (EPlus e1 e2) = case (evalCBN e1) of 
  (EInt n) -> case (evalCBN e2) of
    (EInt m) -> EInt (n+m)
     e2' -> EPlus (EInt n) e2'
  e1' -> case (evalCBN e2) of 
    (EInt m) -> EPlus e1' (EInt m)
    e2' -> EPlus e1' e2'

evalCBN(EMinus e1 e2) = case (evalCBN e1) of
  (EInt n) -> case (evalCBN e2) of
    (EInt m) -> EInt (n-m)
     e2' -> EMinus (EInt n) e2'
  e1' -> case (evalCBN e2) of 
    (EInt m) -> EMinus e1' (EInt m)
    e2' -> EMinus e1' e2'

evalCBN (ETimes e1 e2) = case (evalCBN e1) of
  (EInt n) -> case (evalCBN e2) of 
    (EInt m) -> EInt (n*m)
    e2' -> ETimes (EInt n) e2'
  e1' -> case (evalCBN e2) of 
    (EInt m) ->  ETimes e1' (EInt m)
    e2' -> ETimes e1' e2'

