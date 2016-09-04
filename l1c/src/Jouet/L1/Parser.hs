{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Jouet.L1.Parser (
    binOpP
  , assnOpP
  , identP
  , exprP
  , lvalueP
  , assnP
  ) where

import           Data.Attoparsec.ByteString (Parser, string)
import qualified Data.Attoparsec.ByteString as AB
import           Data.Attoparsec.ByteString.Char8 (char8, decimal, hexadecimal)
import qualified Data.Attoparsec.ByteString.Char8 as ABC

import qualified Data.ByteString as BS

import           Jouet.L1.Grammar
import           Jouet.L1.Parser.Predicate

withSpace :: Parser a -> Parser a
withSpace p = ABC.skipSpace *> p <* ABC.skipSpace

withParens :: Parser a -> Parser a
withParens p =
     withSpace (char8 '(')
  *> p
  <* withSpace (char8 ')')

assnP :: Parser Assn
assnP = Assn <$> identP <*> assnOpP <*> exprP

lvalueP :: Parser Ident
lvalueP = withSpace $ AB.choice [
    identP
  , withParens identP
  ]

exprP :: Parser Expr
exprP = withSpace $ AB.choice [
    exprP'
  , withParens exprP'
  ]

exprP' :: Parser Expr
exprP' = withSpace $ AB.choice [
    intExprP
  , identExprP
  , binaryExprP
  , negativeExprP
  ]

negativeExprP :: Parser Expr
negativeExprP = char8 '-' *> exprP

binaryExprP :: Parser Expr
binaryExprP = BinE <$> exprP <*> binOpP <*> exprP

identExprP :: Parser Expr
identExprP = IdentE <$> identP

intExprP :: Parser Expr
intExprP = IntE <$> (AB.choice [hexInt, decimal])

hexInt :: Parser Int
hexInt = string "0x" *> hexadecimal

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
