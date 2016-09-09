{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Jouet.L1.Parser (
    programP
  , stmtP
  , declP
  , assnP
  , lvalueP
  , exprP
  , identP
  , assnOpP
  , binOpP
  ) where

import           Data.Attoparsec.ByteString (Parser, (<?>), string)
import qualified Data.Attoparsec.ByteString as AB
import           Data.Attoparsec.ByteString.Char8 (char8, decimal, hexadecimal)

import qualified Data.ByteString as BS

import           Jouet.L1.Grammar
import           Jouet.L1.Parser.Helper
import           Jouet.L1.Parser.Predicate

programP :: Parser Program
programP = Program <$> (
     withSpace (string "int")
  *> withSpace (string "main")
  *> withSpace (char8 '{')
  *> AB.many' stmtP
  <* withSpace (char8 '}'))

stmtP :: Parser Stmt
stmtP = (withSpace $ stmtP' <* char8 ';') <?> "stmtP"

stmtP' :: Parser Stmt
stmtP' = withSpace $ AB.choice [
    DeclS <$> declP
  , AssnS <$> assnP
  , ReturnS <$> (withSpace (string "return") *> exprP)
  ]

declP :: Parser Decl
declP = (withSpace $ AB.choice [
    fullDeclP
  , emptyDeclP
  ]) <?> "declP"

fullDeclP :: Parser Decl
fullDeclP =
  IntDecl
    <$> (withSpace (string "int") *> withSpace identP)
    <*> (Just <$> ((char8 '=') *> exprP))

emptyDeclP :: Parser Decl
emptyDeclP = (flip IntDecl Nothing) <$> (
     withSpace (string "int")
  *> identP)

assnP :: Parser Assn
assnP = Assn <$> lvalueP <*> assnOpP <*> exprP <?> "assnP"

lvalueP :: Parser Ident
lvalueP = (withSpace $ AB.choice [
    identP
  , withParens identP
  ]) <?> "lvalueP"

exprP :: Parser Expr
exprP = (withSpace $ AB.choice [
    exprP'
  , withParens exprP'
  ]) <?> "exprP"

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
intExprP = IntE <$> (AB.choice [hexInt, decimal]) <?> "intExprP"

hexInt :: Parser Int
hexInt = string "0x" *> hexadecimal

identP :: Parser Ident
identP =
  Ident
    <$> (BS.cons <$> AB.satisfy identHead <*> AB.takeWhile identTail)
    <?> "identP"

assnOpP :: Parser AssnOp
assnOpP = AB.choice [
    string "+=" *> pure OpAddEq
  , string "-=" *> pure OpSubEq
  , string "*=" *> pure OpMulEq
  , string "/=" *> pure OpDivEq
  , string "%=" *> pure OpModEq
  , char8  '='  *> pure OpEq
  ] <?> "assnOpP"

binOpP :: Parser BinOp
binOpP = AB.choice [
    char8 '+' *> pure OpAdd
  , char8 '-' *> pure OpSub
  , char8 '*' *> pure OpMul
  , char8 '/' *> pure OpDiv
  , char8 '%' *> pure OpMod
  ] <?> "binOpP"
