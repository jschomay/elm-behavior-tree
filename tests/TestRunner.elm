module Main exposing (..)

import ElmTest exposing (..)
import BTree exposing (..)


tests : List Test
tests =
    [ suite "priorityTraverse" priorityTraverseTests
    , test "fail" <| assertNotEqual True False
    ]


type TestAction
    = DoThis
    | DoThat


priorityTraverseTests : List Test
priorityTraverseTests =
    [ test "in ready state"
        <| assertEqual ( [], [ Running, Running, Ready ] )
        <| priorityTraverse 0
            [ Ready, Ready, Ready ]
            []
            [ Goal Action DoThis
            , Goal Action DoThat
            ]
      -- , test "first child running"
      --     <| assertEqual ( [], [Running, Running, Ready] )
      --     <| priorityTraverse 0
      --         [ Ready, Ready, Ready ]
      --         []
      --         [ Goal Action DoThis
      --         , Goal Action DoThat
      --         ]
      -- , test "child succeeds"
      --     <| assertEqual ( [], [Running, Running, Ready] )
      --     <| priorityTraverse 0
      --         [ Ready, Ready, Ready ]
      --         []
      --         [ Goal Action DoThis
      --         , Goal Action DoThat
      --         ]
      -- , test "child fails"
      --     <| assertEqual ( [], [Running, Running, Ready] )
      --     <| priorityTraverse 0
      --         [ Ready, Ready, Ready ]
      --         []
      --         [ Goal Action DoThis
      --         , Goal Action DoThat
      --         ]
      -- , test "second child running"
      --     <| assertEqual ( [], [Running, Running, Ready] )
      --     <| priorityTraverse 0
      --         [ Ready, Ready, Ready ]
      --         []
      --         [ Goal Action DoThis
      --         , Goal Action DoThat
      --         ]
      -- , test "last child succeeds"
      --     <| assertEqual ( [], [Running, Running, Ready] )
      --     <| priorityTraverse 0
      --         [ Ready, Ready, Ready ]
      --         []
      --         [ Goal Action DoThis
      --         , Goal Action DoThat
      --         ]
    ]


consoleTests : Test
consoleTests =
    suite "All Tests" tests


main =
    runSuite consoleTests


type NodeState
    = Ready
    | Running


type BT model msg
    = Node NodeState (NodeType model msg)


type NodeType model msg
    = Select (List BT model msg)
    | Sequence (List BT model msg)
    | Loop (List BT model msg)
    | Concurrent (List BT model msg)
    | Action (model -> msg)
    | Condition (model -> Bool)



-- node constructor functions (instead of using the types directly, like div, h2 etc)
-- define by name, then nest names in tree


select : List (BT model msg) -> BT model msg
select children =
    Node Ready (Select children)


sequence : List BT model msg -> BT model msg
sequence children =
    Node Ready (Sequence children)


concurrent : List BT model msg -> BT model msg
concurrent children =
    Node Ready (Concurrent children)


loop : List BT model msg -> BT model msg
loop children =
    Node Ready (Loop children)


action : (model -> msg) -> BT model msg
action f =
    Node Ready (Action f)


condition : (model -> Bool) -> BT model msg
condition f =
    Node Ready (Condition f)



{- --

   keep the state in the node
   give action nodes a function that gets applied to the state in the tree reducer/mapper/walker

   action nodes pass back up: updated node, nodeOutcome, behaviorMessage

    remove currentBehaviors, just reduce the tree instead
-}
