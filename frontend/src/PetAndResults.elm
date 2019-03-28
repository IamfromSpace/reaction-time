module Main exposing (main)

import Browser exposing (element)
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import Pet
import Platform.Cmd exposing (Cmd)
import Task exposing (perform)
import Time exposing (Posix, now, posixToMillis)


initialModel : Model
initialModel =
    Testing Pet.initialModel


type Model
    = Testing Pet.Model
    | Done Pet.TestResult


type alias Msg =
    Pet.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg s =
    case s of
        Testing ts ->
            let
                ( notifications, ( next, cmds ) ) =
                    Pet.update msg ts
            in
            case notifications of
                Just tr ->
                    ( Done tr, Cmd.map identity cmds )

                Nothing ->
                    ( Testing next, Cmd.map identity cmds )

        _ ->
            ( s, Cmd.none )


view : Model -> Html Msg
view state =
    case state of
        Testing ts ->
            Html.map identity <| Pet.view ts

        Done rs ->
            resultView rs


resultView : Pet.TestResult -> Html a
resultView ({ somewhatHard2, hard, hard2, veryHard } as result) =
    let
        ( slope, intercept ) =
            linearRegression (Pet.toPoints result)

        estimate =
            20 * slope + intercept
    in
    div []
        [ div [] [ text <| "Between Somewhat Hard and Hard: " ++ String.fromFloat somewhatHard2 ++ "s" ]
        , div [] [ text <| "Hard: " ++ String.fromFloat hard ++ "s" ]
        , div [] [ text <| "Between Hard and Very Hard: " ++ String.fromFloat hard2 ++ "s" ]
        , div [] [ text <| "Very Hard: " ++ String.fromFloat veryHard ++ "s" ]
        , div [] [ text <| "Max Time (estimate): " ++ String.fromFloat estimate ++ "s" ]
        ]


linearRegression : List ( Float, Float ) -> ( Float, Float )
linearRegression ps =
    let
        ( xs, ys ) =
            List.unzip ps

        sigmaX =
            List.sum xs

        sigmaY =
            List.sum ys

        sigmaXY =
            List.sum <| List.map (\( x, y ) -> x * y) ps

        sigmaX2 =
            List.sum <| List.map (\x -> x * x) xs

        sigmaY2 =
            List.sum <| List.map (\y -> y * y) ys

        n =
            toFloat <| List.length ps

        slope =
            (n * sigmaXY - sigmaX * sigmaY) / (n * sigmaX2 - sigmaX ^ 2)

        intercept =
            (sigmaY * sigmaX2 - sigmaX * sigmaXY) / (n * sigmaX2 - sigmaX ^ 2)
    in
    ( slope, intercept )


main : Program () Model Msg
main =
    element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
