module Main exposing (..)

import BT exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.App as Html


type Behavior
    = StayHome
    | Eat
    | Search
    | Play


type alias Hunger =
    Int


type alias Loneliness =
    Int


type alias StatusUpdate =
    String


type alias Character =
    { hunger : Hunger
    , loneliness : Loneliness
    , currentBehaviors : List ( NodeId, Behavior )
    , bTree : BTree Behavior
    , bTreeStates : BTreeStates
    , statusUpdates : List StatusUpdate
    }


type alias Day =
    Int


type alias Model =
    { fred : Character
    , days : Day
    }


fredBTree : BTree Behavior
fredBTree =
    Selector Priority
        [ Goal Action StayHome
        , Goal Action Search
        ]


init : Model
init =
    { fred =
        { hunger = 0
        , loneliness = 0
        , currentBehaviors = []
        , bTree = fredBTree
        , bTreeStates = BTree.initBStates fredBTree
        , statusUpdates = [ "Hi, I'm Fred!  I'm just chillin' at home today." ]
        }
    , days = 0
    }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "The Life of Fred" ]
        , h3 [] [ text <| "Day " ++ (toString model.days) ]
        , button [ onClick NewDay ] [ text "New day" ]
        , ul [] (List.map (\status -> li [] [ text status ]) model.fred.statusUpdates)
        ]


type Msg
    = NewDay


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewDay ->
            { model
                | fred = updateCharacter model.fred
                , days = model.days + 1
            }


updateCharacter : Character -> Character
updateCharacter character =
    -- update fred's currentBehaviors based on actions from bTree
    -- update fred's properties based on currentBehaviors
    -- update fred's bTreeStates based on actions that complete
    -- somewhere need to store actions that take multiple days or which day an action expires
    let
        doBehavior : Behavior -> StatusUpdate
        doBehavior behavior =
            case behavior of
                StayHome ->
                    "It's good to be home."

                other ->
                    "Have not yet implemented: " ++ (toString other)

        addStatusUpdates behaviors =
            List.map (snd >> doBehavior) behaviors
                ++ character.statusUpdates

        updateHunger =
            character.hunger + 1

        updateLoneliness =
            if List.member StayHome (List.map snd character.currentBehaviors) then
                character.loneliness + 1
            else
                0

        updateCurrentBehavior : ( NodeId, Behavior ) -> ( NodeId, NodeState )
        updateCurrentBehavior ( nodeId, behavior ) =
            case behavior of
                StayHome ->
                    if character.loneliness > 2 then
                        -- set loneliness to 0 (or do it in updateLoneliness?)
                        -- remove StayHome from currentBehaviors ???
                        ( nodeId, Fail )
                    else
                        -- add 1 to lonliness here??
                        ( nodeId, Running )

                _ ->
                    ( nodeId, Running )

        ( newBehaviors, newBTreeStates ) =
            List.map updateCurrentBehavior character.currentBehaviors
                |> BTree.tick character.bTree (Debug.log "btstates" character.bTreeStates)
    in
        { character
            | statusUpdates = addStatusUpdates (newBehaviors ++ character.currentBehaviors)
            , hunger = updateHunger
            , loneliness = updateLoneliness
            , bTreeStates = newBTreeStates
            , currentBehaviors = (Debug.log "nb" newBehaviors) ++ character.currentBehaviors
        }



{-
   updateing the state of the tree:

   1. newBehaviors is a List (Behavior, NodeIdentifier)
   2. the update funciton first checks for any currentBehaviors that need to be upated, either succeed or fail, making a List (NodeIdentifier, Node Status)
   3. then the update function passes that status update list into BTree.tick
   4. BTree.tick walks the tree, if it hits the NodeIdentifier node it sets the right status for it and its parent nodes as it finishes walking, returning a fully updated BTreeStates, along with newBehaviors
   5. the update funciton continues based off of newBehaviors

   still not sure how to remove behaviors from currentBehaviors...

-}


main : Program Never
main =
    Html.beginnerProgram { model = init, view = view, update = update }



-- see https://github.com/skullzzz/behave
{-
       Tree can be stateless (Unless you want to change the tree at runtime, which is interesting, like learning new behaviors or setting new priorities, or losing abilities...)
       The state of each node would be kept in a separate dict, which both the btree and the main update function would reference.
       - but, what would you use for keys?  The transversal index could work (just an int, or 1.2.1 style) but it seems brittle and makes it impossible to update the tree at runtime.  You could give each node a name, but that wouldn't prevent collisions.

       What is the benefit to not having state in the tree?

       All leaf nodes give commands, even conditionals, which updates their state like normal

    ## Example use:

    Day by Day

    Introduce a character.
    Introduce a day counter, starting at 1.
    Provide a "Next day" button.
    Clicking the button make the character describe what they are doing.

    "It's nice to be home."
    "I'm hungry, let's eat."
    "I'm kind of lonely."
    "Looking for a friend."
    "Let's play."
    "I have to go back home."

    Track stats like hunger, loneliness.
    Looking for a friend lasts for a max of 3 days with a random chance of finding a friend."
    Playing lasts for 2 days.
    Going home takes 1 day.
    Can also have random updates from home like "Watching my favorite TV show", "Reading my book", and "Maybe I'll go for a walk today"

    Hunger and social life are concurrent.
    Stay home then find friend are priorities.
    Looking then play are sequence.
    Play then return home are sequence.

   This could be done with two characters, side by side, who randomly search between 5 shared regions.  If they are in the same region while searching, it succeeds.
-}
