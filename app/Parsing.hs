{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Parsing
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- Module for parsing the CFG of a program according to
-- https://github.com/manuelmontenegro/while_parser_api
module Parsing (CFGParsing (..)) where

import AST (AST (..), Boolean, Numeric)
import CFG (CFG, CFGBlock, CFGBlockContent (..), cfgBlock)
import Control.Applicative ((<|>))
import Control.Monad (join, unless)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import qualified Data.Aeson as J
import Data.Aeson.Types (Parser)
import Data.Coerce (Coercible, coerce)

-- | @'FromJSON' instances declared for newtypes in
-- order to avoid orphan instances.
newtype CFGParsing = CFGParsing CFG

newtype CFGBlockParsing = CFGBlockParsing CFGBlock

newtype CFGBlockContentParsing = CFGBlockContentParsing CFGBlockContent

newtype ASTNumericParsing = ASTNumericParsing (AST Numeric)

newtype ASTBooleanParsing = ASTBooleanParsing (AST Boolean)

instance FromJSON CFGParsing where
  parseJSON = fmap coerce . withObject "CFG" $ \v -> do
    ok <- (.:) v "ok"
    unless ok $ do
      msg <- (.:) @String v "msg"
      fail $ "Error with message: " <> msg
    (.:) @[CFGBlockParsing] v "body"

instance FromJSON CFGBlockParsing where
  parseJSON j = fmap coerce . withObject "CFGBlock" parse $ j
    where
      endSuccs v =
        (.:) @[String] v "succs" >>= \case
          ("end" : _) -> pure []
          _ -> fail "Not the end CGF Block"
      parse v =
        cfgBlock
          <$> v .: "label"
          <*> (endSuccs v <|> (.:) v "succs")
          <*> (coerce <$> parseJSON @CFGBlockContentParsing j)

instance FromJSON CFGBlockContentParsing where
  parseJSON = fmap coerce . withObject "CFGBlockContent" $ \v ->
    (.:) @String v "type" >>= \case
      "skip" -> pure Skip
      "assignment" -> contents v >>= withObject "CFGAssignmentBlock" parseAssignment
      "condition" -> contents v >>= withObject "CFGConditionBlock" parseCondition
      _ -> fail "Unknown block content type"
    where
      contents v = (.:) @J.Value v "contents"
      parseAssignment v =
        Assignment
          <$> v .: "lhs"
          <*> (coerce <$> (.:) @ASTNumericParsing v "rhs")
      parseCondition v =
        Condition
          <$> (coerce <$> (.:) @ASTBooleanParsing v "condition")

instance FromJSON ASTNumericParsing where
  parseJSON = fmap coerce . withObject "ASTNumericExp" . expression $ \case
    "literal" -> withObject "Numeric literal" parseLit
    "variable" -> withObject "Numeric variable" parseVar
    "add" -> withObject "Addition" parseAdd
    "sub" -> withObject "Subtraction" parseSub
    "mul" -> withObject "Multiplication" parseMul
    _ -> const $ fail "Unknown expression type"
    where
      parseLit v = Lit <$> v .: "number"
      parseVar v = Var <$> v .: "name"
      parseAdd = binOp @ASTNumericParsing (:+:)
      parseSub = binOp @ASTNumericParsing (:-:)
      parseMul = binOp @ASTNumericParsing (:*:)

instance FromJSON ASTBooleanParsing where
  parseJSON = fmap coerce . withObject "ASTNumericExp" . expression $ \case
    "leq" -> withObject "Less or equal" parseLeq
    "and" -> withObject "Conjunction" parseAnd
    "eq" -> withObject "Equals" parseEq
    _ -> const $ fail "Unknown expression type"
    where
      parseLeq = binOp @ASTNumericParsing (:<=:)
      parseAnd = binOp @ASTBooleanParsing (:^:)
      parseEq = binOp @ASTNumericParsing (:=:)

-- | Helper function for parsing expressions as
-- tagged subtypes.
expression ::
  (String -> J.Value -> Parser (AST a)) ->
  J.Object ->
  Parser (AST a)
expression taggedParse o = do
  c <- (.:) @String o "category"
  unless (c == "exp") $ fail "Not an expression"
  join $
    taggedParse
      <$> (.:) @String o "category_sub"
      <*> (.:) @J.Value o "options"

-- | Helper function for parsing binary operation
-- expressions.
binOp ::
  forall a b c.
  (FromJSON a, Coercible a b) =>
  (b -> b -> AST c) ->
  J.Object ->
  Parser (AST c)
binOp op o =
  op
    <$> (coerce <$> (.:) @a o "lhs")
    <*> (coerce <$> (.:) @a o "rhs")
