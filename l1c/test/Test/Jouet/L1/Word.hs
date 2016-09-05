{-# LANGUAGE OverloadedStrings #-}

module Test.Jouet.L1.Word where

import           Data.Word (Word8)

alphaUpper :: [Word8]
alphaUpper = [0x41..0x5a]

alphaLower :: [Word8]
alphaLower = [0x61..0x7a]

underscore :: Word8
underscore = 0x5f

digits :: [Word8]
digits = [0x30..0x39]
