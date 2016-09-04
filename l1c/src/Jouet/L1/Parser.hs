{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Jouet.L1.Parser (
    binOpP
  , assnOpP
  , identP
  ) where

import           Data.Attoparsec.ByteString (Parser, string)
import qualified Data.Attoparsec.ByteString as AB
import           Data.Attoparsec.ByteString.Char8 (char8)

import qualified Data.ByteString as BS

import           Data.Word (Word8)

import           Jouet.L1.Grammar
import           Jouet.L1.Parser.Predicate

identP :: Parser Ident
identP =
  Ident
    <$> (BS.cons <$> AB.satisfy identHead <*> AB.takeWhile identTail)

assnOpP :: Parser AssnOp
assnOpP = AB.choice [
    string "+=" *> pure OpAddEq
  , string "-=" *> pure OpSubEq
  , string "*=" *> pure OpMulEq
  , string "/=" *> pure OpDivEq
  , string "%=" *> pure OpModEq
  , char8  '='  *> pure OpEq
  ]

binOpP :: Parser BinOp
binOpP = AB.choice [
    char8 '+' *> pure OpAdd
  , char8 '-' *> pure OpSub
  , char8 '*' *> pure OpMul
  , char8 '/' *> pure OpDiv
  , char8 '%' *> pure OpMod
  ]
