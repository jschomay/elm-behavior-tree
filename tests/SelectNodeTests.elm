module SelectNodeTests exposing (..)

import Test exposing (..)
import Expect exposing (..)
import BT exposing (..)
import MultiwayTreeZipper exposing (..)


selectNodeTests : Test
selectNodeTests =
    describe "for select nodes"
        <| constructing
        ++ tickSelectNode


type TestBehavior
    = DoorNumberOne
    | DoorNumberTwo
    | DoorNumberThree


(&>) =
    Maybe.andThen


constructing : List Test
constructing =
    [ test "constructing and traversing a select node BT with the multiway zipper lib"
        <| \() ->
            let
                mySelect =
                    select
                        [ action DoorNumberOne
                        , action DoorNumberTwo
                        , action DoorNumberThree
                        ]
            in
                Expect.equal (Just DoorNumberThree)
                    <| Just (focus mySelect)
                    &> goToChild 0
                    &> goRight
                    &> goRight
                    &> BT.behavior
    ]


focusedSimpleSelect : FocusedNode TestBehavior
focusedSimpleSelect =
    focus
        <| select
            [ action DoorNumberOne
            , action DoorNumberTwo
            , action DoorNumberThree
            ]


focusedNestedSelect : FocusedNode TestBehavior
focusedNestedSelect =
    focus
        <| select
            [ action DoorNumberOne
            , select [ action DoorNumberTwo ]
            , action DoorNumberThree
            ]


tickSelectNode : List Test
tickSelectNode =
    [ describe "tick"
        [ describe "an invalid state (like a select with no children)"
            [ test "returns the passed in node"
                <| \() ->
                    let
                        incorrectSelectZipper =
                            focus
                                <| select []
                    in
                        Expect.equal (incorrectSelectZipper)
                            <| tick Success incorrectSelectZipper
            ]
        , describe "given a top level select"
            [ test "returns the first action"
                <| \() ->
                    Expect.equal (focusedSimpleSelect |> goToChild 0)
                        <| Just (tick Success focusedSimpleSelect)
            ]
        , describe "given a running child of a select"
            [ test "returns the child right back"
                <| \() ->
                    Expect.equal (focusedSimpleSelect |> goToChild 1)
                        <| Just focusedSimpleSelect
                        &> goToChild 1
                        &> (Just << tick Running)
            ]
        , describe "given any child of a select that succeeds"
            [ test "moves up the tree (resetting to the first action for a top-level select)"
                <| \() ->
                    Expect.equal (focusedSimpleSelect |> goToChild 0)
                        <| Just focusedSimpleSelect
                        &> goToChild 1
                        &> (Just << tick Success)
            ]
        , describe "given a non-last child of a select that fails"
            [ test "returns the next action"
                <| \() ->
                    Expect.equal (focusedSimpleSelect |> goToChild 1)
                        <| Just focusedSimpleSelect
                        &> goToChild 0
                        &> (Just << tick Failure)
            ]
        , describe "given a last child of a select that fails"
            [ test "moves up the tree (resetting to the first action for a top-level select)"
                <| \() ->
                    Expect.equal (focusedSimpleSelect |> goToChild 0)
                        <| Just focusedSimpleSelect
                        &> goToChild 2
                        &> (Just << tick Failure)
            ]
        , describe "with a nested tree"
            [ test "moving down a layer works"
                <| \() ->
                    Expect.equal
                        (Just focusedNestedSelect
                            &> goToChild 1
                            &> goToChild 0
                        )
                        <| Just focusedNestedSelect
                        &> goToChild 0
                        &> (Just << tick Failure)
            , test "moving up a layer works with a failure"
                <| \() ->
                    Expect.equal (focusedNestedSelect |> goToChild 2)
                        <| Just focusedNestedSelect
                        &> goToChild 1
                        &> goToChild 0
                        &> (Just << tick Failure)
            , test "moving up a layer works with a success"
                <| \() ->
                    Expect.equal (focusedNestedSelect |> goToChild 0)
                        <| Just focusedNestedSelect
                        &> goToChild 1
                        &> goToChild 0
                        &> (Just << tick Success)
            ]
        ]
    ]



{-----------
specs and property tests (for select node):
- succeeds if any child succeeds
- fails if all children fails (ie. last child fails)
- running otherwise
- processes children nodes in order from left to right
- if one child succeeds, remaining children don't get evaluated
- at least the first child will be evaluated
- all children will be evaluated if a select node fails
- only one Action returned at a time
- with any valid tree, tick always returns an Action
--}
