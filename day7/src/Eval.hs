module Eval where

import Data.Bits
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data

type WireTable = M.Map WireName Expr
type SeenWireSet = S.Set WireName
type SeenWireList = [WireName]

process :: [Instruction] -> WireTable
process = M.fromList . map (\x -> (dst x, expr x))

data LookupError =
      NoSuchWire WireName
    | CycleDetected [WireName]
  deriving (Eq, Show)

type LookupResult = Either LookupError LiteralValue

lookupWire :: WireTable -> SeenWireSet -> SeenWireList -> WireName -> LookupResult
lookupWire tbl set lst wire =
    if S.member wire set
    then Left $ CycleDetected $ wire : lst
    else case M.lookup wire tbl of
        Nothing -> Left $ NoSuchWire wire
        (Just expr) -> lookupExpr tbl (S.insert wire set) (wire : lst) expr

lookupOperand :: WireTable -> SeenWireSet -> SeenWireList -> Operand -> LookupResult
lookupOperand tbl set lst op = case op of
    (Literal x) -> Right x
    (Wire wire) -> lookupWire tbl set lst wire

lookupExpr :: WireTable -> SeenWireSet -> SeenWireList -> Expr -> LookupResult
lookupExpr tbl set lst expr = case expr of
    (Atom op) -> lop op
    (And op1 op2) -> (.&.) <$> lop op1 <*> lop op2
    (Or op1 op2) -> (.|.) <$> lop op1 <*> lop op2
    (LShift op1 op2) -> shiftL <$> lop op1 <*> (asInt $ lop op2)
    (RShift op1 op2) -> shiftR <$> lop op1 <*> (asInt $ lop op2)
    (Not op) -> complement <$> lop op
  where
    lop :: Operand -> LookupResult
    lop = lookupOperand tbl set lst

    asInt :: LookupResult -> Either LookupError Int
    asInt = fmap (\(LiteralValue w) -> fromIntegral w)

eval :: [Instruction] -> WireName -> LookupResult
eval insts wire = lookupWire (process insts) S.empty [] wire
