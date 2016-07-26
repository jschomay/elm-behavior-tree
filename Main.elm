module Main exposing (..)

import BT
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


type alias DaysOnTheRoad =
    Int


type alias StatusUpdate =
    String



-- type alias Character =
--     { hunger : Hunger
--     , loneliness : Loneliness
--     , daysOnTheRoad : DaysOnTheRoad
--     , bt : CharacterBT
--     , statusUpdates : List StatusUpdate
--     }


type Character
    = Character (BT.BT CharacterData Behavior) CharacterData


type alias CharacterData =
    { hunger : Hunger
    , loneliness : Loneliness
    , daysOnTheRoad : DaysOnTheRoad
    , statusUpdates : List StatusUpdate
    }



-- type CharacterBT
--     = CharacterBT BT.BT Character Behavior


type alias Day =
    Int


type alias Model =
    { fred : Character
    , days : Day
    }


fredBT : BT.BT CharacterData Behavior
fredBT =
    let
        lonely : CharacterData -> BT.NodeOutcome
        lonely { loneliness } =
            if loneliness < 3 then
                BT.Continue
            else
                BT.Fail

        searching : CharacterData -> BT.NodeOutcome
        searching { daysOnTheRoad } =
            if daysOnTheRoad > 3 then
                BT.Fail
                -- else if Time.now `Time.andThen` findAFriend
                -- then BT.Succeed
            else
                BT.Continue

        findAFriend seed =
            False

        -- randomBool seed
    in
        BT.select
            [ BT.action lonely StayHome
            , BT.action searching Search
              -- , BT.action (always BT.Continue) Play -- this needs to be under a sequence with Search
            ]


init : Model
init =
    { fred =
        Character fredBT
            { hunger = 0
            , loneliness = 0
            , daysOnTheRoad = 0
            , statusUpdates = [ "Hi, I'm Fred!  I'm just chillin' at home today." ]
            }
    , days = 0
    }


view : Model -> Html Msg
view model =
    let
        (Character _ { statusUpdates }) =
            model.fred
    in
        div []
            [ h1 [] [ text "The Life of Fred" ]
            , h3 [] [ text <| "Day " ++ (toString model.days) ]
            , button [ onClick NewDay ] [ text "New day" ]
            , ul [] (List.map (\status -> li [] [ text status ]) statusUpdates)
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
updateCharacter (Character bt data) =
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

        addStatusUpdates currentBehaviors =
            List.map doBehavior currentBehaviors
                ++ data.statusUpdates

        updateHunger =
            data.hunger + 1

        updateLoneliness currentBehaviors =
            if List.member StayHome currentBehaviors then
                data.loneliness + 1
            else
                0

        updateDaysOnTheRoad currentBehaviors =
            if List.member Search currentBehaviors then
                data.daysOnTheRoad + 1
            else
                0

        ( updatedBT, currentBehaviors ) =
            BT.updateTree bt data
    in
        Character updatedBT
            { data
                | statusUpdates = addStatusUpdates currentBehaviors
                , hunger = updateHunger
                , daysOnTheRoad = updateDaysOnTheRoad currentBehaviors
                , loneliness = updateLoneliness currentBehaviors
            }


main : Program Never
main =
    Html.beginnerProgram { model = init, view = view, update = update }



-- see https://github.com/skullzzz/behave
{-
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
