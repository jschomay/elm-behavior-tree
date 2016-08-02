module BT
    exposing
        ( focus
        , select
        , sequence
        , action
        , tick
        , behavior
        , BT
        , FocusedNode
        , Outcome(..)
        )

import MultiwayTree exposing (..)
import MultiwayTreeZipper exposing (..)


-- MODEL


type BTNode behavior
    = Select
    | Sequence
    | Action behavior


type Outcome
    = Success
    | Failure
    | Running


type alias BT behavior =
    Tree (BTNode behavior)


type alias BTChidren behavior =
    Forest (BTNode behavior)


select : BTChidren behavior -> BT behavior
select children =
    Tree Select children


sequence : BTChidren behavior -> BT behavior
sequence children =
    Tree Sequence children


action : behavior -> BT behavior
action behavior =
    Tree (Action behavior) []


type alias FocusedNode behavior =
    Zipper (BTNode behavior)


focus : BT behavior -> FocusedNode behavior
focus tree =
    ( tree, [] )


behavior : FocusedNode behavior -> Maybe behavior
behavior focusedNode =
    case MultiwayTreeZipper.datum focusedNode of
        Action behavior ->
            Just behavior

        _ ->
            Nothing


tick : Outcome -> FocusedNode behavior -> FocusedNode behavior
tick outcome focusedNode =
    case goUp focusedNode of
        Nothing ->
            -- we are at root
            firstAction focusedNode

        Just parent ->
            case MultiwayTreeZipper.datum parent of
                Action _ ->
                    -- shouldn't happen, Actions shouldn't have children
                    parent

                Select ->
                    case outcome of
                        Running ->
                            focusedNode

                        Success ->
                            tick Success parent

                        Failure ->
                            Maybe.withDefault (tick Failure parent)
                                <| nextAction focusedNode

                Sequence ->
                    case outcome of
                        Running ->
                            focusedNode

                        Success ->
                            Maybe.withDefault (tick Success parent)
                                <| nextAction focusedNode

                        Failure ->
                            tick Failure parent


(&>) =
    Maybe.andThen


nextAction : FocusedNode behavior -> Maybe (FocusedNode behavior)
nextAction focusedNode =
    goRight focusedNode
        &> (Just << firstAction)


firstAction : FocusedNode behavior -> FocusedNode behavior
firstAction focusedNode =
    -- drop to the first leaf (assumes a valid BT structure)
    Maybe.withDefault focusedNode
        <| goToChild 0 focusedNode
        &> (Just << firstAction)



-- prior art:
-- http://www.gamasutra.com/blogs/ChrisSimpson/20140717/221339/Behavior_trees_for_AI_How_they_work.php
-- http://web.archive.org/web/20140402204854/http://www.altdevblogaday.com/2011/02/24/introduction-to-behavior-trees/
-- https://github.com/skullzzz/behave for simple haskell bt
