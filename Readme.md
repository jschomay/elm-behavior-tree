# Elm Behavior Tree

A way to encode complex decision making into a declarative data structure.

[Learn more about behavior trees](http://www.gamasutra.com/blogs/ChrisSimpson/20140717/221339/Behavior_trees_for_AI_How_they_work.php).

> Still in alpha, looking for feedback, please submit an issue or PR

See `Main.elm` for full example usage / demo.

## Basic usage

Building up a behavior tree:

```elm
type Behavior
    = StayHome
    | Search
    | Propose
    | SettleDown

myBT =
    select
        [ action StayHome
        , sequence
            [ action Search
            , action Propose
            , action SettleDown
            ]
        ]

-- you need to "focus" a node of the tree
{model | currentBTNode = focus myBT}
```

Getting the next behavior:

```elm
outcome = case behavior model.currentBTNode of
    Just StayHome ->
        if model.loneliness > 2 then
            Failure
        else
            Success
    ...

nextBTNode = tick outcome model.currentBTNode
```
