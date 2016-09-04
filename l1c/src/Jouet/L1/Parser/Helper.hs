{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Jouet.L1.Parser.Helper (
    withSpace
  , withParens
  ) where

import           Data.Attoparsec.ByteString (Parser)
import           Data.Attoparsec.ByteString.Char8 (char8)
import qualified Data.Attoparsec.ByteString.Char8 as ABC

withSpace :: Parser a -> Parser a
withSpace p = ABC.skipSpace *> p <* ABC.skipSpace

withParens :: Parser a -> Parser a
withParens p =
     withSpace (char8 '(')
  *> p
  <* withSpace (char8 ')')
