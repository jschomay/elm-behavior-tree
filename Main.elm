module Main exposing (..)

import BT
import Html exposing (..)
import Html.Events exposing (..)
import Html.App as Html


type Behavior
    = NoOp
    | StayHome
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


type alias Character =
    { hunger : Hunger
    , loneliness : Loneliness
    , daysOnTheRoad : DaysOnTheRoad
    , statusUpdates : List StatusUpdate
    , lastAction : Action
    }


type alias Action =
    ( BT.FocusedNode Behavior, BT.Outcome )


type alias Day =
    Int


type alias Model =
    { fred : Character
    , days : Day
    }


fredBT : BT.BT Behavior
fredBT =
    BT.select
        [ BT.action StayHome
        , BT.action Search
        , BT.action Play
        ]


init : Model
init =
    { fred =
        { hunger = 0
        , loneliness = 0
        , daysOnTheRoad = 0
        , statusUpdates = [ "Hi, I'm Fred!  I'm just chillin' at home today." ]
        , lastAction = ( BT.focus fredBT, BT.Running )
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
    let
        doBehavior : Behavior -> BT.Outcome
        doBehavior behavior =
            case behavior of
                NoOp ->
                    BT.Running

                StayHome ->
                    if character.loneliness > 2 then
                        BT.Failure
                    else
                        BT.Success

                Search ->
                    if character.daysOnTheRoad > 2 then
                        BT.Success
                    else
                        BT.Running

                _ ->
                    BT.Failure

        addStatusUpdate currentBehavior outcome =
            case ( currentBehavior, outcome ) of
                ( StayHome, BT.Failure ) ->
                    "I'm tired of staying home, I'm off to have an adventure!"

                ( StayHome, _ ) ->
                    if character.loneliness < 2 then
                        "It's good to be home."
                    else
                        "It's getting kind of lonely..."

                ( Search, BT.Success ) ->
                    "I'm getting tired of all this adventure, time to head back."

                ( Search, _ ) ->
                    "Wow, it's such a big would out here..."

                ( other, _ ) ->
                    "Have not yet implemented: " ++ (toString other)

        updateHunger =
            character.hunger + 1

        updateLoneliness currentBehavior =
            if currentBehavior == StayHome then
                character.loneliness + 1
            else
                0

        updateDaysOnTheRoad currentBehavior =
            if currentBehavior == Search then
                character.daysOnTheRoad + 1
            else
                0

        newFocus =
            let
                ( lastBehavior, lastOutcome ) =
                    character.lastAction
            in
                BT.tick lastOutcome lastBehavior

        currentBehavior =
            Maybe.withDefault NoOp
                <| BT.behavior newFocus

        outcome =
            doBehavior currentBehavior
    in
        { character
            | statusUpdates = (addStatusUpdate currentBehavior outcome) :: character.statusUpdates
            , hunger = updateHunger
            , daysOnTheRoad = updateDaysOnTheRoad currentBehavior
            , loneliness = updateLoneliness currentBehavior
            , lastAction = ( newFocus, outcome )
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
