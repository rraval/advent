import Test.HUnit

import Data
import Eval

main = runTestTT $ TestList
    [ TestCase $ assertEqual "Store and Read"
        (Right $ LiteralValue 16)
        (eval
            [Store (WireName "a") (Atom $ Literal $ LiteralValue 16)]
            (WireName "a")
        )

    , TestCase $ assertEqual "Simple Cycle"
        (Left $ CycleDetected [WireName "x", WireName "y", WireName "x"])
        (eval
            [ Store (WireName "x") (Atom $ Wire $ WireName "y")
            , Store (WireName "y") (Atom $ Wire $ WireName "x")
            ]
            (WireName "x")
        )

    , TestCase $ assertEqual "Invalid Lookup"
        (Left $ NoSuchWire $ WireName "x")
        (eval [] (WireName "x"))
    ]
