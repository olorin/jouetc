{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Jouet.L1.Grammar (
    Expr (..)
  , Decl (..)
  , Assn (..)
  , Ident (..)
  , AssnOp (..)
  , BinOp (..)
  ) where

import           Data.ByteString (ByteString)

data Assn =
    Assn !Ident !AssnOp !Expr
  deriving (Eq, Show)

data Decl =
    IntDecl !Ident !(Maybe Expr)
  deriving (Eq, Show)

newtype Ident =
    Ident ByteString
  deriving (Eq, Show)

data Expr =
    IntE !Int
  | IdentE !Ident
  | BinE !Expr !BinOp !Expr
  | NegE !Expr
  deriving (Eq, Show)

data AssnOp =
    OpEq
  | OpAddEq
  | OpSubEq
  | OpMulEq
  | OpDivEq
  | OpModEq
  deriving (Eq, Show, Enum, Bounded)

data BinOp =
    OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  deriving (Eq, Show, Enum, Bounded)
