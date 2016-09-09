{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

{- Render bits of the grammar. -}
module Jouet.L1.Render (
    renderProgram
  , renderStmt
  , renderExpr
  , renderDecl
  , renderAssn
  , renderIdent
  , renderAssnOp
  , renderBinOp
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Monoid ((<>))

import           Jouet.L1.Grammar

renderProgram :: Program -> ByteString
renderProgram (Program ss) =
  BS.intercalate "\n" $
    ["int main {"] <> (renderStmt <$> ss) <> ["}"]

renderStmt :: Stmt -> ByteString
renderStmt (DeclS s) = renderDecl s <> ";"
renderStmt (AssnS s) = renderAssn s <> ";"
renderStmt (ReturnS s) = "return " <> renderExpr s <> ";"

renderDecl :: Decl -> ByteString
renderDecl (IntDecl i Nothing) = "int " <> renderIdent i
renderDecl (IntDecl i (Just e)) = BS.intercalate " " [
    "int"
  , renderIdent i
  , "="
  , renderExpr e
  ]

renderAssn :: Assn -> ByteString
renderAssn (Assn i o expr) = BS.intercalate " " [
    renderIdent i
  , renderAssnOp o
  , renderExpr expr
  ]

renderExpr :: Expr -> ByteString
renderExpr (IntE n) = BSC.pack $ show n
renderExpr (IdentE i) = renderIdent i
renderExpr (BinE e1 o e2) = BS.intercalate " " [
    renderExpr e1
  , renderBinOp o
  , renderExpr e2
  ]
renderExpr (NegE expr) = "-(" <> renderExpr expr <> ")"

renderIdent :: Ident -> ByteString
renderIdent (Ident x) = x

renderAssnOp :: AssnOp -> ByteString
renderAssnOp OpEq = "="
renderAssnOp OpAddEq = "+="
renderAssnOp OpSubEq = "-="
renderAssnOp OpMulEq = "*="
renderAssnOp OpDivEq = "/="
renderAssnOp OpModEq = "%="

renderBinOp :: BinOp -> ByteString
renderBinOp OpAdd = "+"
renderBinOp OpSub = "-"
renderBinOp OpMul = "*"
renderBinOp OpDiv = "/"
renderBinOp OpMod = "%"
