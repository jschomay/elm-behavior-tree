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
    = DoorNumberOne
    | DoorNumberTwo
    | DoorNumberThree


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
                            (Select [ Node Ready (Action succeed DoorNumberOne) ])

                    updatedSelect =
                        Node Ready
                            (Select [ Node Ready (Action succeed DoorNumberOne) ])
                in
                    Expect.equal ( updatedSelect, [ DoorNumberOne ], Succeed )
                        <| processNode initialSelect testModel
        , test "that fails"
            <| \() ->
                let
                    fail =
                        always Fail

                    initialSelect =
                        Node Ready
                            (Select [ Node Ready (Action fail DoorNumberOne) ])

                    updatedSelect =
                        Node Ready
                            (Select [ Node Ready (Action fail DoorNumberOne) ])
                in
                    Expect.equal ( updatedSelect, [], Fail )
                        <| processNode initialSelect testModel
        , test "that continues"
            <| \() ->
                let
                    continue =
                        always Continue

                    initialSelect =
                        Node Ready
                            (Select [ Node Ready (Action continue DoorNumberOne) ])

                    updatedSelect =
                        Node Running
                            (Select [ Node Running (Action continue DoorNumberOne) ])
                in
                    Expect.equal ( updatedSelect, [ DoorNumberOne ], Continue )
                        <| processNode initialSelect testModel
        ]
    , describe "with multiple children"
        [ test "first child succeeds"
            <| \() ->
                let
                    succeed =
                        always Succeed

                    fail =
                        always Fail

                    continue =
                        always Continue

                    initialSelect =
                        Node Ready
                            (Select
                                [ Node Ready (Action succeed DoorNumberOne)
                                , Node Ready (Action continue DoorNumberTwo)
                                , Node Ready (Action fail DoorNumberThree)
                                ]
                            )

                    updatedSelect =
                        Node Ready
                            (Select
                                [ Node Ready (Action succeed DoorNumberOne)
                                , Node Ready (Action continue DoorNumberTwo)
                                , Node Ready (Action fail DoorNumberThree)
                                ]
                            )
                in
                    Expect.equal ( updatedSelect, [ DoorNumberOne ], Succeed )
                        <| processNode initialSelect testModel
        , test "second child was running and succeeds"
            <| \() ->
                let
                    succeed =
                        always Succeed

                    fail =
                        always Fail

                    continue =
                        always Continue

                    initialSelect =
                        Node Running
                            (Select
                                [ Node Ready (Action succeed DoorNumberOne)
                                , Node Running (Action succeed DoorNumberTwo)
                                , Node Ready (Action fail DoorNumberThree)
                                ]
                            )

                    updatedSelect =
                        Node Ready
                            (Select
                                [ Node Ready (Action succeed DoorNumberOne)
                                , Node Ready (Action succeed DoorNumberTwo)
                                , Node Ready (Action fail DoorNumberThree)
                                ]
                            )
                in
                    Expect.equal ( updatedSelect, [ DoorNumberTwo ], Succeed )
                        <| processNode initialSelect testModel
        , test "second child was running and fails"
            <| \() ->
                let
                    succeed =
                        always Succeed

                    fail =
                        always Fail

                    continue =
                        always Continue

                    initialSelect =
                        Node Running
                            (Select
                                [ Node Ready (Action succeed DoorNumberOne)
                                , Node Running (Action fail DoorNumberTwo)
                                , Node Ready (Action fail DoorNumberThree)
                                ]
                            )

                    updatedSelect =
                        Node Ready
                            (Select
                                [ Node Ready (Action succeed DoorNumberOne)
                                , Node Ready (Action fail DoorNumberTwo)
                                , Node Ready (Action fail DoorNumberThree)
                                ]
                            )
                in
                    Expect.equal ( updatedSelect, [], Fail )
                        <| processNode initialSelect testModel
        , test "third child running"
            <| \() ->
                let
                    succeed =
                        always Succeed

                    fail =
                        always Fail

                    continue =
                        always Continue

                    initialSelect =
                        Node Running
                            (Select
                                [ Node Ready (Action succeed DoorNumberOne)
                                , Node Ready (Action continue DoorNumberTwo)
                                , Node Running (Action continue DoorNumberThree)
                                ]
                            )

                    updatedSelect =
                        Node Running
                            (Select
                                [ Node Ready (Action succeed DoorNumberOne)
                                , Node Ready (Action continue DoorNumberTwo)
                                , Node Running (Action continue DoorNumberThree)
                                ]
                            )
                in
                    Expect.equal ( updatedSelect, [ DoorNumberThree ], Continue )
                        <| processNode initialSelect testModel
        , test "last child fails"
            <| \() ->
                let
                    succeed =
                        always Succeed

                    fail =
                        always Fail

                    continue =
                        always Continue

                    initialSelect =
                        Node Ready
                            (Select
                                [ Node Ready (Action fail DoorNumberOne)
                                , Node Ready (Action fail DoorNumberTwo)
                                , Node Ready (Action fail DoorNumberThree)
                                ]
                            )

                    updatedSelect =
                        Node Ready
                            (Select
                                [ Node Ready (Action fail DoorNumberOne)
                                , Node Ready (Action fail DoorNumberTwo)
                                , Node Ready (Action fail DoorNumberThree)
                                ]
                            )
                in
                    Expect.equal ( updatedSelect, [], Fail )
                        <| processNode initialSelect testModel
        ]
    ]



{-----------
property tests (for select node):
- if all children fail select fails (alternatively, if last child fails, select fails)
- if one child succeeds node succeeds
- if one child succeeds, remaining children don't get evaluated
- if one child fails, remaining children get evaluated until all fail
- only one child running at a time
- if no children are concurrent nodes, only one msg will get returned for a select
--}
