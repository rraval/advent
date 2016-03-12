import Test.HUnit

import Lib

main = runTestTT $ TestList
    [ TestLabel "first" $ TestList
        [ TestCase $ count "" @?= 0

        , TestCase $ count "(())" @?= 0
        , TestCase $ count "()()" @?= 0

        , TestCase $ count "(((" @?= 3
        , TestCase $ count "(()(()(" @?= 3

        , TestCase $ count "))(((((" @?= 3

        , TestCase $ count "())" @?= -1
        , TestCase $ count "))(" @?= -1

        , TestCase $ count ")))" @?= -3
        , TestCase $ count ")())())" @?= -3
        ]

    , TestLabel "second" $ TestList
        [ TestCase $ findPos 0 "" @=? Just 0
        , TestCase $ findPos 1 "" @=? Nothing

        , TestCase $ findPos (-1) ")" @=? Just 1
        , TestCase $ findPos (-1) "()())" @=? Just 5

        , TestCase $ findPos (-1) "()()(()))(" @=? Just 9
        ]
    ]
