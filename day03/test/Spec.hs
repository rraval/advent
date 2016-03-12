import qualified Data.Map.Strict as M
import Test.HUnit

import Lib

ex1 = [DirRight]
ex2 = [DirUp, DirRight, DirDown, DirLeft]
ex3 =
    [ DirUp, DirDown
    , DirUp, DirDown
    , DirUp, DirDown
    , DirUp, DirDown
    , DirUp, DirDown
    ]

main = runTestTT $ TestList
    [ TestLabel "deliver" $ TestList
        [ deliverPresents [] ~?= DeliveryState
            { currentHouse = House 0 0
            , houseMap = M.fromList [(House 0 0, PresentCount 1)]
            }

        , deliverPresents ex1 ~?= DeliveryState
            { currentHouse = House 1 0
            , houseMap = M.fromList
                [ (House 0 0, PresentCount 1)
                , (House 1 0, PresentCount 1)
                ]
            }

        , deliverPresents ex2 ~?= DeliveryState
            { currentHouse = House 0 0
            , houseMap = M.fromList
                [ (House 0 0, PresentCount 2)
                , (House 0 1, PresentCount 1)
                , (House 1 1, PresentCount 1)
                , (House 1 0, PresentCount 1)
                ]
            }

        , deliverPresents ex3 ~?= DeliveryState
            { currentHouse = House 0 0
            , houseMap = M.fromList
                [ (House 0 0, PresentCount 6)
                , (House 0 1, PresentCount 5)
                ]
            }
        ]

    , TestLabel "parse" $ TestList
        [ parseDirList "" ~?= []
        , parseDirList ">" ~?= ex1
        , parseDirList "^>v<" ~?= ex2
        , parseDirList "^v^v^v^v^v" ~=? ex3
        ]
    ]
