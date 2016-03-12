{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit

import Lib

main = runTestTT $ TestList
    [ mine "abcdef" 5 ~?= 609043
    , mine "pqrstuv" 5 ~?= 1048970
    ]
