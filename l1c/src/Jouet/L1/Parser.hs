{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Jouet.L1.Parser (
    binOpP
  ) where

import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (char8)

import           Jouet.L1.Grammar

binOpP :: Parser BinOp
binOpP = choice [
    char8 '+' *> pure OpAdd
  , char8 '-' *> pure OpSub
  , char8 '*' *> pure OpMul
  , char8 '/' *> pure OpDiv
  , char8 '%' *> pure OpMod
  ]
