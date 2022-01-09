{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : MonotoneFramework
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- This module defines an abstract @'MonotoneFramework'
-- which can be instanciated in order to perform
-- different analysis over program Control Flow Graphs.
module MonotoneFramework
  ( MonotoneFramework (..),
    BlockAnalysis (..),
    backwardAnalysis,
    forwardAnalysis,
  )
where

import CFG
  ( CFG,
    CFGBlock (..),
    cfgBlock,
    maxLabel,
    minLabel,
    succs,
  )
import Control.Monad (forM_)
import Data.Array (Array, array, bounds, (!))
import Data.Array.ST (MArray (newArray), readArray, runSTArray, writeArray)
import Data.List (foldl')
import Numeric.Natural (Natural)

-- | A monotone framework consists of an associated
-- analysis type, an extremal element of this type,
-- a lowest upper bound between two elements
-- of this type, and a transfer function when given a block.
class Eq (L mf) => MonotoneFramework mf where
  type L mf
  extremal :: L mf
  lub :: L mf -> L mf -> L mf
  transfer :: CFGBlock -> L mf -> L mf

-- | This data type holds the result of analysing a
-- block, both in its inputs and outputs.
data BlockAnalysis a = BlockAnalysis
  { block :: CFGBlock,
    inData :: a,
    outData :: a
  }

instance Show a => Show (BlockAnalysis a) where
  show b =
    show (block b)
      <> "\n  In analysis: "
      <> show (inData b)
      <> "\n  Out analysis: "
      <> show (outData b)

-- | Type alias for an analysis function.
type Analysis mf =
  MonotoneFramework mf =>
  CFG ->
  [BlockAnalysis (L mf)]

-- | Type alias for an analysis transformation step.
type FlowStep mf =
  MonotoneFramework mf =>
  CFG ->
  Array Natural (L mf) ->
  Array Natural (L mf)

-- | Given a monotone framework instance well defined
-- for a specific analysis, this performs it in
-- forwards over a CFG.
forwardAnalysis :: forall mf. MonotoneFramework mf => Analysis mf
forwardAnalysis bs =
  reversedAnalysis
    . backwardAnalysis @mf
    . reversedCFG
    $ bs
  where
    reversedAnalysis = zipWith setBlock bs
    setBlock b ab =
      BlockAnalysis
        b
        (outData ab)
        (inData ab)

-- | Given a monotone framework instance well defined
-- for a specific analysis, this performs it in
-- backwards over a CFG.
backwardAnalysis :: forall mf. MonotoneFramework mf => Analysis mf
backwardAnalysis bs =
  gather
    . fst
    . head
    . filter (uncurry (==))
    . zip chain
    $ tail chain
  where
    start = initial @mf bs
    chain = iterate step (start, start)
    step (is, os) =
      ( backwardIns @mf bs os,
        backwardOuts @mf bs is
      )
    gather (is, os) =
      [ BlockAnalysis b (is ! label b) (os ! label b)
        | b <- bs
      ]

-- | Inverts the control flow of a CFG.
reversedCFG :: CFG -> CFG
reversedCFG bs = reversedBlock <$> bs
  where
    preds = runSTArray $ do
      ps <- newArray (minLabel bs, maxLabel bs) []
      forM_ bs $ \b ->
        forM_ (succs b) $ \s -> do
          cs <- readArray ps s
          writeArray ps s $ label b : cs
      pure ps
    reversedBlock b =
      cfgBlock
        (label b)
        (preds ! label b)
        (content b)

-- | An array filled with extremal values indexed by
-- each block label.
initial ::
  forall mf.
  MonotoneFramework mf =>
  CFG ->
  Array Natural (L mf)
initial bs =
  array
    (minLabel bs, maxLabel bs)
    [ (label b, e)
      | b <- bs
    ]
  where
    e = extremal @mf

backwardIns :: forall mf. FlowStep mf
backwardIns bs os =
  array
    (bounds os)
    [ (label b, transfer @mf b $ os ! label b)
      | b <- bs
    ]

backwardOuts :: forall mf. FlowStep mf
backwardOuts bs is =
  array
    (bounds is)
    [ (label b, out $ succs b)
      | b <- bs
    ]
  where
    out [] = extremal @mf
    out (c : cs) =
      foldl' (lub @mf) (is ! c) $
        (is !) <$> cs
