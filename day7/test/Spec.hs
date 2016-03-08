import Test.HUnit

import Data
import Eval

main = runTestTT $ TestList
    [ TestCase $ assertEqual "Store and Read"
        (Right $ LiteralValue 16)
        (eval
            [Store { dst = WireName "a", expr = Atom $ Literal $ LiteralValue 16 }]
            (WireName "a")
        )
    ]
