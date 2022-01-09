{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- Module      : AST
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- This module offers a data type for an AST of the
-- 'while' language expressions, which can be @'Numeric'
-- or @'Boolean'.
module AST
  ( AST (..),
    Numeric,
    Boolean,
    vars,
  )
where

import Variables (Variables, singleton)

-- | Kind of expression types, with a corresponding
-- associated type for literals.
data Type
  = Numeric
  | Boolean

type Numeric = 'Numeric

type Boolean = 'Boolean

-- | Instead of defining two different data types
-- and composing them, we are using a GADT approach
-- for defining a single AST with polymorphically
-- constrained constructors.
data AST (a :: Type) where
  Lit :: Int -> AST Numeric
  Var :: String -> AST Numeric
  (:+:) :: AST Numeric -> AST Numeric -> AST Numeric
  (:-:) :: AST Numeric -> AST Numeric -> AST Numeric
  (:*:) :: AST Numeric -> AST Numeric -> AST Numeric
  (:<=:) :: AST Numeric -> AST Numeric -> AST Boolean
  (:=:) :: AST Numeric -> AST Numeric -> AST Boolean
  (:^:) :: AST Boolean -> AST Boolean -> AST Boolean

infixl 6 :+:

infixl 6 :-:

infixl 7 :*:

infixr 3 :^:

instance Show (AST a) where
  show (Lit v) = show v
  show (Var x) = x
  show (e1 :+: e2) = binOpShow "+" e1 e2
  show (e1 :-: e2) = binOpShow "-" e1 e2
  show (e1 :*: e2) = binOpShow "*" e1 e2
  show (e1 :<=: e2) = binOpShow "<=" e1 e2
  show (e1 :=: e2) = binOpShow "==" e1 e2
  show (e1 :^: e2) = binOpShow "&&" e1 e2

-- | Obtain the set of variables appearing
-- in an AST expression.
vars :: AST a -> Variables
vars (Lit _) = mempty
vars (Var v) = singleton v
vars (e1 :+: e2) = vars e1 <> vars e2
vars (e1 :-: e2) = vars e1 <> vars e2
vars (e1 :*: e2) = vars e1 <> vars e2
vars (e1 :<=: e2) = vars e1 <> vars e2
vars (e1 :=: e2) = vars e1 <> vars e2
vars (e1 :^: e2) = vars e1 <> vars e2

binOpShow :: String -> AST a -> AST a -> String
binOpShow s e1 e2 =
  "(" <> show e1 <> " " <> s <> " " <> show e2 <> ")"
