module PomsButtons exposing (boxes)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, preventDefaultOn)
import Json.Decode exposing (succeed)
import Poms exposing (Score, allScores, showScore)


box : String -> Score -> Html Score
box color score =
    Html.div
        [ style "height" "16vw"
        , style "width" "16vw"
        , style "text-align" "center"
        , style "font-size" "3vw"
        , style "line-height" "16vw"
        , style "border" "1px solid black"
        , style "background-color" color
        , preventDefaultOn "touchstart" (succeed ( score, True ))
        , onClick score
        ]
        [ Html.text (showScore score) ]


justMatch : Maybe a -> a -> Bool
justMatch ma b =
    case ma of
        Just a ->
            a == b

        Nothing ->
            False


boxes : Maybe Score -> Html Score
boxes selected =
    Html.div
        [ style "flex-direction" "row", style "display" "flex" ]
        (List.map
            (\score ->
                box
                    (if justMatch selected score then
                        "#23ba2d"

                     else
                        "#FFF"
                    )
                    score
            )
            allScores
        )
