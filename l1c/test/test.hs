{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Applicative (pure)

import           Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  pure ()
