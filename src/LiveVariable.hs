{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : LiveVariable
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- This module contains an instance of @'MonotoneFramework'
-- which defines an analysis of program live variables.
module LiveVariable where

import AST (vars)
import qualified CFG
import MonotoneFramework (MonotoneFramework (..))
import Variables (Variables, delete)

-- | Void type for carrying the instance declaration.
data LiveVariable

instance MonotoneFramework LiveVariable where
  type L LiveVariable = Variables
  extremal = mempty
  lub = (<>)
  transfer b l =
    case CFG.content b of
      CFG.Skip -> l
      CFG.Condition e -> l <> vars e
      CFG.Assignment v e -> delete v l <> vars e
