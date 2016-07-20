module TestRunnerHtml exposing (..)

import Test exposing (concat)
import Test.Runner.Html
import AllTests exposing (allTests)


main : Program Never
main =
    [ allTests ]
        |> concat
        |> Test.Runner.Html.run
