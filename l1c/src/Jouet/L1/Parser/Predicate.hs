{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Jouet.L1.Parser.Predicate (
    identHead
  , identTail
  ) where

import           Data.Word (Word8)

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
