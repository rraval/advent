import Test.HUnit

import Lib

main = runTestTT $ TestList
    [ TestLabel "first" $ TestList
        [ count "" ~?= 0

        , count "(())" ~?= 0
        , count "()()" ~?= 0

        , count "(((" ~?= 3
        , count "(()(()(" ~?= 3

        , count "))(((((" ~?= 3

        , count "())" ~?= -1
        , count "))(" ~?= -1

        , count ")))" ~?= -3
        , count ")())())" ~?= -3
        ]

    , TestLabel "second" $ TestList
        [ findPos 0 "" ~?= Just 0
        , findPos 1 "" ~?= Nothing

        , findPos (-1) ")" ~?= Just 1
        , findPos (-1) "()())" ~?= Just 5

        , findPos (-1) "()()(()))(" ~?= Just 9
        ]
    ]
