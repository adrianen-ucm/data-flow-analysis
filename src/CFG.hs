{-# LANGUAGE GADTs #-}

-- |
-- Module      : CFG
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- This module defines a data type for the Control Flow
-- Graph of a program.
module CFG
  ( CFG,
    CFGBlock,
    CFGBlockContent (..),
    cfgBlock,
    label,
    succs,
    content,
    maxLabel,
    minLabel,
  )
where

import AST (AST, Boolean, Numeric)
import Data.List (intersperse)
import qualified Data.Set as S
import Numeric.Natural (Natural)

-- | A CFG is a sequence of blocks.
type CFG = [CFGBlock]

-- | Each block is labelled and has some
-- successors, or none if it is a final block.
data CFGBlock = CFGBlock
  { label :: Natural,
    succsSet :: S.Set Natural,
    content :: CFGBlockContent
  }

-- | Smart constructor for a block.
cfgBlock :: Natural -> [Natural] -> CFGBlockContent -> CFGBlock
cfgBlock l = CFGBlock l . S.fromList

-- | Smart accessor for the block successors.
succs :: CFGBlock -> [Natural]
succs = S.toList . succsSet

instance Show CFGBlock where
  show b =
    "Block " <> (show . label) b
      <> ":\n  Content: "
      <> show (content b)
      <> "\n  Successors: "
      <> (successors . succs) b
    where
      successors [] = "None"
      successors s =
        mconcat . intersperse ", " $
          show <$> s

-- | A block can represent a skip node, an assignment node
-- | or a condition node.
data CFGBlockContent where
  Skip :: CFGBlockContent
  Assignment :: String -> AST Numeric -> CFGBlockContent
  Condition :: AST Boolean -> CFGBlockContent

instance Show CFGBlockContent where
  show Skip = "Skip"
  show (Assignment x e) = x <> ":= " <> show e
  show (Condition e) = "Cond " <> show e

-- | Find the greatest label of a CFG. We are arbitrarily
-- defining it to be 0 when the CFG is empty.
maxLabel :: CFG -> Natural
maxLabel [] = 0
maxLabel bs = maximum $ label <$> bs

-- | Find the least label of a CFG. We are arbitrarily
-- defining it to be 0 when the CFG is empty.
minLabel :: CFG -> Natural
minLabel [] = 0
minLabel bs = minimum $ label <$> bs
