module TestRunner exposing (..)

import Html.App
import Html exposing (text)
import Test exposing (..)
import Test.Runner.Log
import AllTests exposing (allTests)


main : Program Never
main =
    Html.App.beginnerProgram
        { model = ()
        , update = \_ _ -> ()
        , view = \() -> Html.text "Check the console for useful output!"
        }
        |> Test.Runner.Log.run (Test.concat [ allTests ])
