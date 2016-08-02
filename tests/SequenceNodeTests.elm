module SequenceNodeTests exposing (..)

import Test exposing (..)
import Expect exposing (..)
import BT exposing (..)
import MultiwayTreeZipper exposing (..)


sequenceNodeTests : Test
sequenceNodeTests =
    describe "for sequence nodes"
        <| constructing
        ++ tickSequenceNode


type TestBehavior
    = DoorNumberOne
    | DoorNumberTwo
    | DoorNumberThree


(&>) =
    Maybe.andThen


constructing : List Test
constructing =
    [ test "constructing and traversing a sequence node BT with the multiway zipper lib"
        <| \() ->
            let
                mySequence =
                    sequence
                        [ action DoorNumberOne
                        , action DoorNumberTwo
                        , action DoorNumberThree
                        ]
            in
                Expect.equal (Just DoorNumberThree)
                    <| Just (focus mySequence)
                    &> goToChild 0
                    &> goRight
                    &> goRight
                    &> BT.behavior
    ]


focusedSimpleSequence : FocusedNode TestBehavior
focusedSimpleSequence =
    focus
        <| sequence
            [ action DoorNumberOne
            , action DoorNumberTwo
            , action DoorNumberThree
            ]


focusedNestedSequence : FocusedNode TestBehavior
focusedNestedSequence =
    focus
        <| sequence
            [ action DoorNumberOne
            , sequence [ action DoorNumberTwo ]
            , action DoorNumberThree
            ]


tickSequenceNode : List Test
tickSequenceNode =
    [ describe "tick"
        [ describe "an invalid state (like a sequence with no children)"
            [ test "returns the passed in node"
                <| \() ->
                    let
                        incorrectSequenceZipper =
                            focus
                                <| sequence []
                    in
                        Expect.equal (incorrectSequenceZipper)
                            <| tick Success incorrectSequenceZipper
            ]
        , describe "given a top level sequence"
            [ test "returns the first action"
                <| \() ->
                    Expect.equal (focusedSimpleSequence |> goToChild 0)
                        <| Just (tick Success focusedSimpleSequence)
            ]
        , describe "given a running child of a sequence"
            [ test "returns the child right back"
                <| \() ->
                    Expect.equal (focusedSimpleSequence |> goToChild 1)
                        <| Just focusedSimpleSequence
                        &> goToChild 1
                        &> (Just << tick Running)
            ]
        , describe "given any child of a sequence that fails"
            [ test "moves up the tree (resetting to the first action for a top-level sequence)"
                <| \() ->
                    Expect.equal (focusedSimpleSequence |> goToChild 0)
                        <| Just focusedSimpleSequence
                        &> goToChild 1
                        &> (Just << tick Failure)
            ]
        , describe "given a non-last child of a sequence that succeeds"
            [ test "returns the next action"
                <| \() ->
                    Expect.equal (focusedSimpleSequence |> goToChild 1)
                        <| Just focusedSimpleSequence
                        &> goToChild 0
                        &> (Just << tick Success)
            ]
        , describe "given a last child of a sequence that succeeds"
            [ test "moves up the tree (resetting to the first action for a top-level sequence)"
                <| \() ->
                    Expect.equal (focusedSimpleSequence |> goToChild 0)
                        <| Just focusedSimpleSequence
                        &> goToChild 2
                        &> (Just << tick Success)
            ]
        , describe "with a nested tree"
            [ test "moving down a layer works"
                <| \() ->
                    Expect.equal
                        (Just focusedNestedSequence
                            &> goToChild 1
                            &> goToChild 0
                        )
                        <| Just focusedNestedSequence
                        &> goToChild 0
                        &> (Just << tick Success)
            , test "moving up a layer with a failure fails (resets)"
                <| \() ->
                    Expect.equal (focusedNestedSequence |> goToChild 0)
                        <| Just focusedNestedSequence
                        &> goToChild 1
                        &> goToChild 0
                        &> (Just << tick Failure)
            , test "moving up a layer with a success suceeds (next child)"
                <| \() ->
                    Expect.equal (focusedNestedSequence |> goToChild 2)
                        <| Just focusedNestedSequence
                        &> goToChild 1
                        &> goToChild 0
                        &> (Just << tick Success)
            ]
        ]
    ]



{-----------
specs and property tests (for sequence node):
- fails if any child fails
- moves to next child when child succeeds
- succeeds when last child succeeds
- running otherwise
- processes children nodes in order from left to right
- if one child fails, remaining children don't get evaluated
- at least the first child will be evaluated
- all children will be evaluated if a sequence node succeeds
- only one Action returned at a time
- with any valid tree, tick always returns an Action
--}
