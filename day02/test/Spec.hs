import Test.HUnit

import Lib

present1 = Cuboid (Length 2) (Width 3) (Height 4)
present2 = Cuboid (Length 1) (Width 1) (Height 10)

main = runTestTT $ TestList
    [ TestLabel "wrapping" $ TestList
        [ 58 ~=? wrapping present1
        , 43 ~=? wrapping present2
        ]

    , TestLabel "parsing" $ TestList
        [ Just present1 ~=? parsePresent "2x3x4"
        , Just present2 ~=? parsePresent "1x1x10"
        , Nothing ~=? parsePresent "1x1"
        ]
    ]
