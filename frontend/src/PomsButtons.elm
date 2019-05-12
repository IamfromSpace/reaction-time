module PomsButtons exposing (boxes)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, preventDefaultOn)
import Json.Decode exposing (succeed)
import Poms exposing (Score, allScores, showScore)


box : Score -> Html Score
box score =
    Html.div
        [ style "height" "16vw"
        , style "width" "16vw"
        , style "text-align" "center"
        , style "font-size" "3vw"
        , style "line-height" "16vw"
        , style "border" "1px solid black"
        , preventDefaultOn "touchstart" (succeed ( score, True ))
        , onClick score
        ]
        [ Html.text (showScore score) ]


boxes : Html Score
boxes =
    Html.div
        [ style "flex-direction" "row", style "display" "flex" ]
        (List.map box allScores)
