{-# LANGUAGE OverloadedStrings #-}

module Test.Jouet.L1.Gen where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Monoid ((<>))

import           Jouet.L1.Grammar

import           Test.Jouet.L1.Word
import           Test.QuickCheck

genProgram :: Gen Program
genProgram = Program <$> listOf genStmt

genStmt :: Gen Stmt
genStmt = oneof [
    DeclS <$> genDecl
  , AssnS <$> genAssn
  , ReturnS <$> genExpr
  ]

genAssn :: Gen Assn
genAssn =
  Assn
    <$> genIdent
    <*> genAssnOp
    <*> genExpr

genDecl :: Gen Decl
genDecl =
  IntDecl
    <$> genIdent
    <*> (oneof [pure Nothing, Just <$> genExpr])

genExpr :: Gen Expr
genExpr = sized genExpr'

genExpr' :: Int -> Gen Expr
genExpr' 0 = oneof [
    IntE <$> choose (0, (2^32) - 1)
  , IdentE <$> genIdent
  ]
genExpr' n = oneof [
    IntE <$> choose (0, (2^32) - 1)
  , IdentE <$> genIdent
  , BinE <$> genExpr' (n - 1) <*> genBinOp <*> genExpr' (n - 1)
  , NegE <$> genExpr' (n - 1)
  ]
    
genIdent :: Gen Ident
genIdent = Ident <$> (
      BS.cons
  <$> elements (alphaUpper <> alphaLower <> [underscore])
  <*> (BS.pack <$>
          listOf (elements (
               alphaUpper
            <> alphaLower
            <> digits
            <> [underscore]))))

genAssnOp :: Gen AssnOp
genAssnOp = elements [minBound..maxBound]

genBinOp :: Gen BinOp
genBinOp = elements [minBound..maxBound]
