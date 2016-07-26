module BT exposing (..)


type BT model msg
    = Node NodeState (NodeType model msg)


type alias NodeList model msg =
    List (BT model msg)


type NodeType model msg
    = Select (NodeList model msg)
    | Sequence (NodeList model msg)
    | Loop (NodeList model msg)
    | Concurrent (NodeList model msg)
    | Action (model -> NodeOutcome) msg


type NodeState
    = Ready
    | Running


type NodeOutcome
    = Fail
    | Succeed
    | Continue


type alias ProcessNodeResult model msg =
    ( BT model msg, List msg, NodeOutcome )


select : List (BT model msg) -> BT model msg
select children =
    Node Ready (Select children)


sequence : List (BT model msg) -> BT model msg
sequence children =
    Node Ready (Sequence children)


concurrent : List (BT model msg) -> BT model msg
concurrent children =
    Node Ready (Concurrent children)


loop : List (BT model msg) -> BT model msg
loop children =
    Node Ready (Loop children)


action : (model -> NodeOutcome) -> msg -> BT model msg
action condition msg =
    Node Ready (Action condition msg)


updateTree : BT model msg -> model -> ( BT model msg, List msg )
updateTree bt model =
    let
        ( updatedBt, msgs, _ ) =
            processNode bt model
    in
        ( updatedBt, msgs )


processNode :
    BT model msg
    -> model
    -> ProcessNodeResult model msg
processNode node model =
    case node of
        Node nodeState (Select children) ->
            processSelectNode nodeState children model

        Node nodeState (Action condition msg) ->
            processActionNode nodeState condition msg model

        _ ->
            ( node, [], Continue )


processActionNode :
    NodeState
    -> (model -> NodeOutcome)
    -> msg
    -> model
    -> ProcessNodeResult model msg
processActionNode nodeState condition msg model =
    case condition model of
        Continue ->
            ( Node Running (Action condition msg), [ msg ], Continue )

        Succeed ->
            ( Node Ready (Action condition msg), [ msg ], Succeed )

        Fail ->
            ( Node Ready (Action condition msg), [], Fail )


processSelectNode :
    NodeState
    -> List (BT model msg)
    -> model
    -> ProcessNodeResult model msg
processSelectNode nodeState children model =
    case children of
        [] ->
            -- selects should have children, so no-op
            ( Node Ready (Select children), [], Succeed )

        [ onlyChild ] ->
            case processNode onlyChild model of
                ( updatedChild, msgs, Continue ) ->
                    ( Node Running (Select [ updatedChild ]), msgs, Continue )

                ( updatedChild, msgs, Succeed ) ->
                    ( Node Ready (Select [ updatedChild ]), msgs, Succeed )

                ( updatedChild, msgs, Fail ) ->
                    ( Node Ready (Select [ updatedChild ]), msgs, Fail )

        _ ->
            let
                updateChildren : List (BT model msg) -> model -> ( List (BT model msg), List msg, NodeOutcome )
                updateChildren children model =
                    case children of
                        [] ->
                            ( [], [], Succeed )

                        [ onlyChild ] ->
                            let
                                ( updatedChild, msgs, outcome ) =
                                    processNode onlyChild model
                            in
                                ( [ updatedChild ], msgs, outcome )

                        firstChild :: remainingChildren ->
                            let
                                ( updatedFirstChild, firstChildMsgs, firstChildOutcome ) =
                                    processNode firstChild model
                            in
                                case firstChildOutcome of
                                    Fail ->
                                        let
                                            ( updatedRemaingingChildren, remainingChildrenMsgs, remainingChildrenOutcome ) =
                                                updateChildren remainingChildren model
                                        in
                                            ( updatedFirstChild :: updatedRemaingingChildren, remainingChildrenMsgs, remainingChildrenOutcome )

                                    _ ->
                                        ( updatedFirstChild :: remainingChildren, firstChildMsgs, firstChildOutcome )

                updateChildrenFromFirstRunning : List (BT model msg) -> model -> ( List (BT model msg), List msg, NodeOutcome )
                updateChildrenFromFirstRunning children model =
                    case children of
                        ((Node Ready _) as firstChild) :: remainingChildren ->
                            let
                                ( updatedRemaingingChildren, remainingChildrenMsgs, remainingChildrenOutcome ) =
                                    updateChildrenFromFirstRunning remainingChildren model
                            in
                                ( firstChild :: updatedRemaingingChildren, remainingChildrenMsgs, remainingChildrenOutcome )

                        _ ->
                            updateChildren children model

                updateNode : ( List (BT model msg), List msg, NodeOutcome ) -> ProcessNodeResult model msg
                updateNode ( updatedChildren, msgs, outcome ) =
                    ( Node
                        (if outcome == Continue then
                            Running
                         else
                            Ready
                        )
                        (Select updatedChildren)
                    , msgs
                    , outcome
                    )
            in
                case nodeState of
                    Running ->
                        updateNode <| updateChildrenFromFirstRunning children model

                    Ready ->
                        updateNode <| updateChildren children model



-- prior art:
-- http://web.archive.org/web/20140402204854/http://www.altdevblogaday.com/2011/02/24/introduction-to-behavior-trees/
-- https://github.com/skullzzz/behave for simple haskell bt
