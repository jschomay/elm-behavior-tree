module BTree exposing (..)

import Dict
import List


type SelectorType
    = Priority
    | Concurrent
    | Sequence
    | Loop
    | Random


type GoalType
    = Condition
    | Action


type BTree cmd
    = Selector SelectorType (List (BTree cmd))
    | Goal GoalType cmd


type NodeState
    = Ready
    | Running
    | Success
    | Fail

type NewBehavior cmd =
  Do (NodeId, cmd)
    | None

type alias NodeName =
    String


type alias BTreeStates =
    List NodeState


type alias NodeId =
    Int


tick : BTree cmd -> BTreeStates -> List ( NodeId, NodeState ) -> ( List ( NodeId, cmd ), BTreeStates )
tick bTree bTreeStates changedStates =
    let

        traverse : NodeId -> SelectorType -> List (BTree cmd) -> ( List ( NodeId, cmd ), BTreeStates )
        traverse nodeId selectorType children =
            -- each layer is responible for setting the state of all of its children and returning any batched commands, gathered children states, and its own state
            -- it will choose which of its children to run in order to accomplish this before returning (recursively), based on its SelectorType


        execute : NodeId -> GoalType -> cmd -> (NewBehavior, NodeState)
        execute nodeId goalType cmd =
            -- if state is ready return (Do(id, cmd), Running)
            -- if changedStates has this id, return (None, newState)
            -- if state is running return (None, Running)

            (  Do ( nodeId, cmd ) , Running )
    in
      case bTree of
          Selector selectorType children ->
              traverse 0 selectorType children

          Goal goalType cmd ->
            -- really shouldn't happen - a tree of just an action
            ([0,cmd],[Running])


initBStates : BTree cmd -> BTreeStates
initBStates bTree =
    -- todo, generate from tree
    [ Ready, Ready, Ready ]



-- case children of
--     [] ->
--         Debug.crash "You must give a selector node children"
--     x :: xs ->
--         (tick x) :: List.map tick xs
-- see https://github.com/skullzzz/behave for simple haskell btree
-- and http://web.archive.org/web/20140402204854/http://www.altdevblogaday.com/2011/02/24/introduction-to-behavior-trees/
{-
   Tree can be stateless (Unless you want to change the tree at runtime, which is interesting, like learning new behaviors or setting new priorities, or losing abilities...)
   The state of each node would be kept in a separate dict, which both the btree and the main update function would reference.
   - but, what would you use for keys?  The transversal index could work (just an int, or 1.2.1 style) but it seems brittle and makes it impossible to update the tree at runtime.  You could give each node a name, but that wouldn't prevent collisions.

   What is the benefit to not having state in the tree?

   All leaf nodes give commands, even conditionals, which updates their state like normal
-}
