module AllTests exposing (..)

import Test exposing (..)
import Expect exposing (..)
import BT exposing (..)
import SelectNodeTests exposing (selectNodeTests)


allTests : Test
allTests =
    describe "BT"
        [ selectNodeTests
        , general
        ]


type TestAction
    = TestAction


general : Test
general =
    describe "general"
        [ describe "behavior"
            [ test "for select nodes gives nothing"
                <| \() ->
                    Expect.equal Nothing (behavior <| focus <| select [])
            , test "for action nodes gives action"
                <| \() ->
                    Expect.equal (Just TestAction) (behavior <| focus <| action TestAction)
            ]
        ]
