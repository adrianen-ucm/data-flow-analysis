{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Main
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- This program expects some 'while' source code via
-- stdin, sends it to an API for obtaining its CFG
-- and performs several static analysis (i.e. live variable
-- and constant propagation).
module Main where

import ConstantPropagation (ConstantPropagation)
import Control.Exception (evaluate)
import Data.Coerce (coerce)
import LiveVariable (LiveVariable)
import MonotoneFramework (backwardAnalysis, forwardAnalysis)
import Network.HTTP.Req (Req, (/:), (=:))
import qualified Network.HTTP.Req as R
import Parsing (CFGParsing (..))

-- | Usage example:
--
--     cat example.while | cabal run
--
-- You can also use:
--
--     cabal run
--
-- type by hand the source code and
-- press CTRL+D when finished.
main :: IO ()
main = do
  putStrLn "Waiting for the source code input..."
  code <- getContents
  _ <- evaluate $ length code
  putStrLn "Obtaining the program CFG..."
  cfg <- fmap coerce . R.runReq R.defaultHttpConfig . cfgRequest $ code
  _ <- evaluate $ length cfg
  putStrLn "Live variable analysis:\n"
  mapM_ print . backwardAnalysis @LiveVariable $ cfg
  putStrLn "Constant propagation analysis:\n"
  mapM_ print . forwardAnalysis @ConstantPropagation $ cfg

-- | CFG request according to
-- https://github.com/manuelmontenegro/while_parser_api
cfgRequest :: String -> Req CFGParsing
cfgRequest code =
  R.responseBody
    <$> R.req
      R.POST
      (R.http "dalila.sip.ucm.es" /: "while_parser" /: "api" /: "parse")
      (R.ReqBodyUrlEnc $ "cfg" =: True <> "while_code" =: code)
      R.jsonResponse
      (R.port 4000)
