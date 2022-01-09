{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Semantics
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- This module provides a simple evaluation meaning
-- for numerical expressions via denotational semantics.
module Semantics
  ( State,
    get,
    set,
    unset,
    common,
    eval,
  )
where

import AST (AST (..), Numeric)
import Data.Coerce (coerce)
import Data.List (intersperse)
import qualified Data.Map as M

-- | It is implemented as a newtype around @'M.Map String Int'
-- with a custom @'Show' instance.
newtype State = State (M.Map String Int)
  deriving (Eq, Semigroup, Monoid)

instance Show State where
  show (State st) =
    brackets
      . mconcat
      . intersperse ", "
      . map mapsTo
      . M.toList
      $ st
    where
      mapsTo (v, n) = v <> " -> " <> show n
      brackets s = "{" <> s <> "}"

-- | Get the value of a variable if present.
get :: String -> State -> Maybe Int
get v = M.lookup v . coerce

-- | Set the value of a variable.
set :: String -> Int -> State -> State
set v n = coerce . M.insert v n . coerce

-- | Unset the value of a variable.
unset :: String -> State -> State
unset v = State . M.delete v . coerce

-- | Combine two states only in its common values.
common :: State -> State -> State
common s1 =
  State
    . M.mapMaybe combine
    . M.intersectionWith (,) (coerce s1)
    . coerce
  where
    combine (v1, v2) =
      if v1 == v2
        then Just v1
        else Nothing

eval :: AST Numeric -> State -> Maybe Int
eval (Lit n) _ = Just n
eval (Var v) s = get v s
eval (e1 :+: e2) s = (+) <$> eval e1 s <*> eval e2 s
eval (e1 :-: e2) s = (-) <$> eval e1 s <*> eval e2 s
eval (e1 :*: e2) s = (*) <$> eval e1 s <*> eval e2 s
