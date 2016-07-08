module Main exposing (..)

import BTree exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.App as Html


type Behavior
    = AtHome
    | Eat
    | Search
    | Play



-- btree : BTree Behavior
-- bTree =
--     Selector Concurrent
--         [ Goal Action SayHi
--         , Selector Sequence
--             [ Goal Condition Wait
--             , Goal Action SayBye
--             ]
--         ]


type alias Hunger =
    Int


type alias Loneliness =
    Int


type alias StatusUpdate =
    String


type alias Character =
    { hunger : Hunger
    , loneliness : Loneliness
    , currentBehaviors : List Behavior
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
    Goal Action AtHome


init : Model
init =
    { fred =
        { hunger = 0
        , loneliness = 0
        , currentBehaviors = [ AtHome ]
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
            -- update fred's stats
            -- update fred's status based on actions from his bTree
            -- update fred's bTreeStates based on actions that complete
            -- somewhere need to store actions that take multiple days or which day an action expires
            { model
                | fred = updateCharacter model.fred
                , days = model.days + 1
            }


updateCharacter : Character -> Character
updateCharacter character =
    let
        doBehavior : Behavior -> StatusUpdate
        doBehavior behavior =
            case behavior of
                AtHome ->
                    "It's good to be home."

                other ->
                    "Have not yet implemented: " ++ (toString other)

        addStatusUpdates =
            List.map doBehavior character.currentBehaviors
                ++ character.statusUpdates
    in
        { character
            | statusUpdates = addStatusUpdates
        }


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
