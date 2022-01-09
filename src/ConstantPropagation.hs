{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : ConstantPropagation
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- This module contains an instance of @'MonotoneFramework'
-- which defines an analysis of constant propagation.
module ConstantPropagation where

import qualified CFG
import MonotoneFramework (MonotoneFramework (..))
import Semantics (State, common, eval, set, unset)

-- | Void type for carrying the instance declaration.
data ConstantPropagation

instance MonotoneFramework ConstantPropagation where
  type L ConstantPropagation = State
  extremal = mempty
  lub = common
  transfer b l =
    case CFG.content b of
      CFG.Skip -> l
      CFG.Condition _ -> l
      CFG.Assignment v e -> case eval e l of
        Nothing -> unset v l
        Just n -> set v n l
