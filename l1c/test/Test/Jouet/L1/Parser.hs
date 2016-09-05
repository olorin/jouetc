{-# LANGUAGE OverloadedStrings #-}

module Test.Jouet.L1.Parser where

import qualified Data.Attoparsec.ByteString as AB

import           Jouet.L1.Grammar
import           Jouet.L1.Parser
import           Jouet.L1.Render

import           Test.Jouet.L1.Gen
import           Test.QuickCheck

tripping :: Property
tripping = forAll genProgram $ \p ->
  let
    bs = renderProgram p
    p' = AB.parseOnly programP bs
  in
  Right p === p'
