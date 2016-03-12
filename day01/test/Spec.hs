import Test.HUnit

import Lib

main = runTestTT $ TestList
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
