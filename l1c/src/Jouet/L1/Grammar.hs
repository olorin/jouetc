{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Jouet.L1.Grammar (
    AssnOp (..)
  , BinOp (..)
  ) where

data AssnOp =
    OpEq
  | OpAddEq
  | OpSubEq
  | OpMulEq
  | OpDivEq
  | OpModEq

data BinOp =
    OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  deriving (Eq, Show, Enum, Bounded)
