import Test.HUnit
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
            (runParser "42 -> a")

        , TestCase $ assertEqual "Atom: Wire"
            (Just $ Store (Atom $ Wire $ WireName "y") (WireName "a"))
            (runParser "y -> a")

        , TestCase $ assertEqual "And"
            (Just $ Store (And (Wire $ WireName "x") (Literal $ LiteralValue 15)) (WireName "a"))
            (runParser "x AND 15 -> a")

        , TestCase $ assertEqual "Or"
            (Just $ Store (Or (Wire $ WireName "x") (Literal $ LiteralValue 15)) (WireName "a"))
            (runParser "x OR 15 -> a")

        , TestCase $ assertEqual "LShift"
            (Just $ Store (LShift (Wire $ WireName "x") (Literal $ LiteralValue 15)) (WireName "a"))
            (runParser "x LSHIFT 15 -> a")

        , TestCase $ assertEqual "RShift"
            (Just $ Store (RShift (Wire $ WireName "x") (Literal $ LiteralValue 15)) (WireName "a"))
            (runParser "x RSHIFT 15 -> a")

        , TestCase $ assertEqual "Not"
            (Just $ Store (Not $ Literal $ LiteralValue 15) (WireName "a"))
            (runParser "NOT 15 -> a")
        ]
    ]

runParser :: String -> Maybe Instruction
runParser s = case parseString instructionParser mempty s of
    (Success i) -> Just i
    _ -> Nothing
