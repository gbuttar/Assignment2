-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language LambdaNat1.

module AbsLambdaNat where

import Prelude (String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Program = Prog Exp
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Exp = EAbs Id Exp | EApp Exp Exp | EVar Id
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Id = Id String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)
