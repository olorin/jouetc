{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Applicative (pure)

import           Test.Hspec (hspec)
import           Test.Hspec.QuickCheck (prop)
import qualified Test.Jouet.L1.Parser as Parser

main :: IO ()
main = hspec $ do
  prop "parser round-trips" Parser.tripping
