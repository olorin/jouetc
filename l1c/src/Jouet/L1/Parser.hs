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

identP :: Parser Ident
identP = Ident <$> (BS.cons <$> AB.satisfy identHead <*> AB.takeWhile identTail)

identTail :: Word8 -> Bool
identTail 0x5f = True -- underscore
identTail w = or [
    isAlphaLower w
  , isAlphaUpper w
  , isDigit w
  ]

identHead :: Word8 -> Bool
identHead 0x5f = True -- underscore
identHead w = or [
    isAlphaLower w
  , isAlphaUpper w
  ]

isDigit :: Word8 -> Bool
isDigit w = w >= 0x30 && w <= 0x39

isAlphaUpper :: Word8 -> Bool
isAlphaUpper w = w >= 0x41 && w <= 0x5a

isAlphaLower :: Word8 -> Bool
isAlphaLower w = w >= 0x61 && w <= 0x7a

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
