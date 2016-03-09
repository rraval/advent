import Test.HUnit
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Text.Trifecta

import Data
import Eval
import Parser

main = runTestTT $ TestList
    [ TestLabel "Evaluation" $ TestList
        [ TestCase $ assertEqual "Store and Read"
            (Right $ LiteralValue 16)
            (eval
                [Store (Atom $ Literal $ LiteralValue 16) (WireName "a")]
                (WireName "a")
            )

        , TestCase $ assertEqual "Simple Cycle"
            (Left $ CycleDetected [WireName "x", WireName "y", WireName "x"])
            (eval
                [ Store (Atom $ Wire $ WireName "y") (WireName "x")
                , Store (Atom $ Wire $ WireName "x") (WireName "y")
                ]
                (WireName "x")
            )

        , TestCase $ assertEqual "Invalid Lookup"
            (Left $ NoSuchWire $ WireName "x")
            (eval [] (WireName "x"))
        ]

    , TestLabel "Parsing" $ TestList
        [ TestCase $ assertEqual "Atom: Literal"
            (Just $ Store (Atom $ Literal $ LiteralValue 42) (WireName "a"))
            (runParser instructionParser "42 -> a")

        , TestCase $ assertEqual "Atom: Wire"
            (Just $ Store (Atom $ Wire $ WireName "y") (WireName "a"))
            (runParser instructionParser "y -> a")

        , TestCase $ assertEqual "And"
            (Just $ Store (And (Wire $ WireName "x") (Literal $ LiteralValue 15)) (WireName "a"))
            (runParser instructionParser "x AND 15 -> a")

        , TestCase $ assertEqual "Or"
            (Just $ Store (Or (Wire $ WireName "x") (Literal $ LiteralValue 15)) (WireName "a"))
            (runParser instructionParser "x OR 15 -> a")

        , TestCase $ assertEqual "LShift"
            (Just $ Store (LShift (Wire $ WireName "x") (Literal $ LiteralValue 15)) (WireName "a"))
            (runParser instructionParser "x LSHIFT 15 -> a")

        , TestCase $ assertEqual "RShift"
            (Just $ Store (RShift (Wire $ WireName "x") (Literal $ LiteralValue 15)) (WireName "a"))
            (runParser instructionParser "x RSHIFT 15 -> a")

        , TestCase $ assertEqual "Not"
            (Just $ Store (Not $ Literal $ LiteralValue 15) (WireName "a"))
            (runParser instructionParser "NOT 15 -> a")
        ]

    , TestLabel "Program" $ TestList
        [ TestLabel "Simple" $ TestCase $ assertProgram
            (LiteralValue 7)
            (WireName "result")
                " 0 -> x                \n\
                \ x OR 7 -> y           \n\
                \ y AND 15 -> result    \n"

        , TestLabel "Example" $ TestCase $ assertProgram
            (LiteralValue 65079)
            (WireName "i")
                " 123 -> x              \n\
                \ 456 -> y              \n\
                \ x AND y -> d          \n\
                \ x OR y -> e           \n\
                \ x LSHIFT 2 -> f       \n\
                \ y RSHIFT 2 -> g       \n\
                \ NOT x -> h            \n\
                \ NOT y -> i            \n"
        ]
    ]

runParser :: Parser a -> String -> Maybe a
runParser p s = case parseString p mempty s of
    (Success i) -> Just i
    _ -> Nothing

data ProgramResult =
      ParseError Doc
    | EvalError LookupError
    | ProgramResult LiteralValue
  deriving (Show)

runProgram :: WireName -> String -> ProgramResult
runProgram wire prog = case parseString instructionListParser mempty prog of
    (Failure doc) -> ParseError doc
    (Success insts) -> case eval insts wire of
        (Left err) -> EvalError err
        (Right val) -> ProgramResult val

assertProgram :: LiteralValue -> WireName -> String -> Assertion
assertProgram result wire prog = case runProgram wire prog of
    (ParseError doc) -> assertFailure (show doc)
    (EvalError err) -> assertFailure (show err)
    (ProgramResult val) -> assertEqual "" val result
