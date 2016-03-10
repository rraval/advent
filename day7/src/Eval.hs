module Eval where

import Control.Monad.Trans.State.Strict
import Data.Bits
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data

type WireTable = M.Map WireName Expr
type SeenWireSet = S.Set WireName
type SeenWireList = [WireName]
type WireCache = M.Map WireName LiteralValue

process :: [Instruction] -> WireTable
process = M.fromList . map (\(Store e d) -> (d, e))

data LookupError =
      NoSuchWire WireName
    | CycleDetected [WireName]
  deriving (Eq, Show)

type LookupResult = Either LookupError LiteralValue

type LookupState = State WireCache LookupResult

lookupWire :: WireTable -> SeenWireSet -> SeenWireList -> WireName -> LookupState
lookupWire tbl set lst wire = do
    cache <- get
    case M.lookup wire cache of
        (Just val) -> return $ Right val
        Nothing ->
            if S.member wire set
            then return $ Left $ CycleDetected $ wire : lst
            else case M.lookup wire tbl of
                Nothing -> return $ Left $ NoSuchWire wire
                (Just expr) -> do
                    result <- lookupExpr tbl (S.insert wire set) (wire : lst) expr
                    case result of
                        p@(Left err) -> return p
                        p@(Right val) -> do
                            modify $ M.insert wire val
                            return p

lookupOperand :: WireTable -> SeenWireSet -> SeenWireList -> Operand -> LookupState
lookupOperand tbl set lst op = case op of
    (Literal x) -> return $ Right x
    (Wire wire) -> lookupWire tbl set lst wire

lookupExpr :: WireTable -> SeenWireSet -> SeenWireList -> Expr -> LookupState
lookupExpr tbl set lst expr = case expr of
    (Atom op) -> lop op
    (Not op) -> complement `fmap2` lop op
    (And op1 op2) -> (.&.) `fmap2` lop op1 `ap2` lop op2
    (Or op1 op2) -> (.|.) `fmap2` lop op1 `ap2` lop op2
    (LShift op1 op2) -> shiftL `fmap2` lop op1 `ap2` (asInt `fmap2` lop op2)
    (RShift op1 op2) -> shiftR `fmap2` lop op1 `ap2` (asInt `fmap2` lop op2)
  where
    lop :: Operand -> LookupState
    lop = lookupOperand tbl set lst

    fmap2 :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)
    fmap2 = fmap . fmap

    ap2 :: (Applicative f, Applicative f1) => f (f1 (a -> b)) -> f (f1 a) -> f (f1 b)
    ap2 func val = (<*>) <$> func <*> val

    asInt :: LiteralValue -> Int
    asInt (LiteralValue w) = fromIntegral w

eval :: [Instruction] -> WireName -> LookupResult
eval insts wire = evalState (lookupWire (process insts) S.empty [] wire) M.empty
