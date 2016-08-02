module Main exposing (..)

import BT
import Html exposing (..)
import Html.Events exposing (..)
import Html.App as Html
import Random exposing (initialSeed, step, Seed, int)


type Behavior
    = NoOp
    | StayHome
    | Search
    | Propose
    | SettleDown


type alias Loneliness =
    Int


type alias DaysOnTheRoad =
    Int


type alias StatusUpdate =
    String


type alias Character =
    { loneliness : Loneliness
    , daysOnTheRoad : DaysOnTheRoad
    , statusUpdates : List StatusUpdate
    , lastAction : Action
    , seed : Seed
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
        , BT.sequence
            [ BT.action Search
            , BT.action Propose
            , BT.action SettleDown
            ]
        ]


init : Model
init =
    { fred =
        { loneliness = 0
        , daysOnTheRoad = 0
        , statusUpdates = [ "Hi, I'm Fred!  I'm just chillin' at home today." ]
        , lastAction = ( BT.focus fredBT, BT.Running )
        , seed = initialSeed 1
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
        ( randomInt, newSeed ) =
            step (int 0 3) character.seed

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
                    if randomInt == 2 then
                        BT.Success
                    else if character.daysOnTheRoad < 2 then
                        BT.Running
                    else
                        BT.Failure

                Propose ->
                    if snd character.lastAction /= BT.Running then
                        BT.Running
                    else if randomInt == 2 then
                        BT.Success
                    else
                        BT.Failure

                SettleDown ->
                    BT.Running

        addStatusUpdate currentBehavior outcome =
            case ( currentBehavior, outcome ) of
                ( StayHome, BT.Failure ) ->
                    "I'm tired of staying home, I'm off to have an adventure!"

                ( StayHome, _ ) ->
                    let
                        lastBehavior =
                            ( character.lastAction |> fst >> BT.behavior, character.lastAction |> snd )
                    in
                        if lastBehavior == ( Just Propose, BT.Failure ) then
                            "Oooh the pain of rejection!  I'm going back home."
                        else if character.loneliness == 0 then
                            "Home sweet, home."
                        else if character.loneliness < 2 then
                            "It's good to be home."
                        else
                            "It's getting kind of lonely..."

                ( Search, BT.Running ) ->
                    "Wow, it's such a big would out here..."

                ( Search, BT.Failure ) ->
                    "I'm getting tired of all this adventure, time to head back."

                ( Search, BT.Success ) ->
                    "Hey, I found a friend!"

                ( Propose, BT.Running ) ->
                    "Hi, I'm Fred... will you marry me?"

                ( Propose, BT.Success ) ->
                    "Yes!  You're who I've been looking for my whole life!"

                ( Propose, BT.Failure ) ->
                    "Eww, no, go away."

                ( SettleDown, _ ) ->
                    "And they lived happily ever after..."

                ( _, _ ) ->
                    "Um... what was I doing?"

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
            , daysOnTheRoad = updateDaysOnTheRoad currentBehavior
            , loneliness = updateLoneliness currentBehavior
            , lastAction = ( newFocus, outcome )
            , seed = newSeed
        }


main : Program Never
main =
    Html.beginnerProgram { model = init, view = view, update = update }



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
