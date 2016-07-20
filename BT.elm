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



-- ( Node Running (Action action), [ action model ], Continue )


processSelectNode :
    NodeState
    -> List (BT model msg)
    -> model
    -> ProcessNodeResult model msg
processSelectNode nodeState children model =
    case children of
        [] ->
            -- selects should have children, so no-op
            ( Node Ready (Select children), [], Continue )

        [ onlyChild ] ->
            case processNode onlyChild model of
                ( updatedChild, msgs, Continue ) ->
                    ( Node Running (Select [ updatedChild ]), msgs, Continue )

                ( updatedChild, msgs, Succeed ) ->
                    ( Node Ready (Select [ updatedChild ]), msgs, Succeed )

                ( updatedChild, msgs, Fail ) ->
                    ( Node Ready (Select [ updatedChild ]), msgs, Fail )

        firstChild :: remainingChildren ->
            -- TODO continue filling out correct returns here...
            ( Node Ready (Select children), [], Continue )



{-
    updateNode : BT -> (BT, List msg, NodeOutcome)
      (first defers to nodeType implementation, below is selector type)
      case no children
        -> error/noop

      case only one child
        -> just update the child node

      case update first child (running/running or ready/ready)
        ->
          let (updatedChilden, msgs, outcome) =
                updateChildren children
          in
            (Node (setNodeState outcome) Select updatedChilden
            , msgs
            , setNodeOutcome outcome
            )

      case otherwise
        ->
          let (updatedChildren, msgs, outcome) =
                updateChildrenFromFirstRunning children
          in
            (Node (setNodeState outcome) Select updatedChildren
            , msgs
            , setNodeOutcome outcome
            )

   updateChildrenFromFirstRunning : List BT -> (List BT, List msg, NodeOutcome)
      case empty -> error/([], [], Succeed)
      case one child -> (updateNode child)
      case head :: tail
       case head is running -> updateChildren children
       case otherwise -> (head :: updateChildrenFromFirstRunning tail, msgsFromTail, outcomeFromTail)

    updateChildren : List BT -> (List BT, List msg, NodeOutcome)
      case empty -> error/([], [], Succeed)
      case one child -> (updateNode child)
      case head :: tail ->
        update head
          case continue -> (updatedHead :: tail, headMsgs, Continue)
          case succeed -> (updatedHead :: tail, headMsgs, Succeed)
          case fail -> (updatedHead :: updateChildren tail, tailMsgs, tailOucome)


    # to process a node (of selector type):
      - update current node children
        - pick child to update based on node state and node type
          - update child state and return any msgs and outcome
          - if node fails
            - update next child based on outcome and node rules
      - update node state based on outcome and node rules
      - return node outcome based on child outcome and rules
      - return msgs


    # combinators:

    combine : (BT, List msg) -> List msg -> (Bt, List msg)

    append : (BT, List msg) -> (List BT, List msg) -> (BT, List msg)


    --
       give action nodes a function that gets applied to the state in the tree reducer/mapper/walker

       action nodes pass back up: updated node, nodeOutcome, behaviorMessage

        remove currentBehaviors, just reduce the tree instead
-}
-- case children of
--     [] ->
--         Debug.crash "You must give a selector node children"
--     x :: xs ->
--         (tick x) :: List.map tick xs
-- see https://github.com/skullzzz/behave for simple haskell btree
-- and http://web.archive.org/web/20140402204854/http://www.altdevblogaday.com/2011/02/24/introduction-to-behavior-trees/
