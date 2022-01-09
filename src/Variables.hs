{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- |
-- Module      : Variables
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- This module defines a data structure for handling a set
-- of variables.
module Variables
  ( Variables,
    singleton,
    delete,
  )
where

import Data.Coerce (coerce)
import Data.List (intersperse)
import qualified Data.Set as S

-- | It is implemented as a newtype around @'Data.Set String'
-- with a custom @'Show' instance.
newtype Variables = Variables (S.Set String)
  deriving (Eq, Semigroup, Monoid)

instance Show Variables where
  show =
    brackets
      . mconcat
      . intersperse ", "
      . S.toList
      . coerce
    where
      brackets s = "{" <> s <> "}"

-- | A set of a single variable.
singleton :: String -> Variables
singleton = coerce . S.singleton

-- | Remove a variable from the set.
delete :: String -> Variables -> Variables
delete v vs =
  coerce $
    S.delete v (coerce vs)
