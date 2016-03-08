{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data where

import Data.Bits (Bits)
import Data.Word (Word16)

newtype WireName = WireName String deriving (Eq, Show, Ord)
newtype LiteralValue = LiteralValue Word16 deriving (Eq, Show, Ord, Bits)

data Operand =
      Wire WireName
    | Literal LiteralValue

data Expr =
      Atom Operand
    | And Operand Operand
    | Or Operand Operand
    | LShift Operand Operand
    | RShift Operand Operand
    | Not Operand

data Instruction = Store { dst :: WireName, expr :: Expr }
