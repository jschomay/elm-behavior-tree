module AllTests exposing (..)

import Test exposing (..)
import Expect exposing (..)
import BT exposing (..)


allTests : Test
allTests =
    describe "update"
        [ describe "for select nodes"
            updateSelectTests
        ]


type TestAction
    = DoThis
    | DoThat


type alias TestModel =
    Bool


testModel : TestModel
testModel =
    True


updateSelectTests : List Test
updateSelectTests =
    [ describe "with only one child"
        [ test "that succeeds"
            <| \() ->
                let
                    succeed =
                        always Succeed

                    initialSelect =
                        Node Ready
                            (Select [ Node Ready (Action succeed DoThis) ])

                    updatedSelect =
                        Node Ready
                            (Select [ Node Ready (Action succeed DoThis) ])
                in
                    Expect.equal ( updatedSelect, [ DoThis ] )
                        <| updateTree initialSelect testModel
        , test "that fails"
            <| \() ->
                let
                    fail =
                        always Fail

                    initialSelect =
                        Node Ready
                            (Select [ Node Ready (Action fail DoThis) ])

                    updatedSelect =
                        Node Ready
                            (Select [ Node Ready (Action fail DoThis) ])
                in
                    Expect.equal ( updatedSelect, [] )
                        <| updateTree initialSelect testModel
        , test "that continues"
            <| \() ->
                let
                    continue =
                        always Continue

                    initialSelect =
                        Node Ready
                            (Select [ Node Ready (Action continue DoThis) ])

                    updatedSelect =
                        Node Running
                            (Select [ Node Running (Action continue DoThis) ])
                in
                    Expect.equal ( updatedSelect, [ DoThis ] )
                        <| updateTree initialSelect testModel
        ]
    ]



-- todo - only child tests for fail and succeed
-- [ test "select ready and all children ready"
--     <| \() ->
--         let
--             updatedSelect =
--                 Node Running
--                     (Select
--                         [ Node Running (Action thisAction)
--                         , Node Ready (Action thatAction)
--                         ]
--                     )
--         in
--             Expect.equal ( updatedSelect, [ DoThis ] )
--                 <| updateTree initialSelect testModel
-- ]
{-----------
property tests (for select node):
- if all children fail select fails (alternatively, if last child fails, select fails)
- if one child succeeds node succeeds
- if one child succeeds, remaining children don't get evaluated
- if one child fails, remaining children get evaluated until all fail
- only one child running at a time
--}
