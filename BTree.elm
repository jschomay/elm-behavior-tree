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


type alias NodeName =
    String


type alias BTreeStates =
    List NodeState



-- tick : BTree cmd -> BTreeStates -> ( NodeState, BTreeStates, List cmd )


tick node bTreeStates =
    bTreeStates


initBStates : BTree cmd -> BTreeStates
initBStates bTree =
    [ Ready ]



-- case children of
--     [] ->
--         Debug.crash "You must give a selector node chilrdren"
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
