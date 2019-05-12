module PomsTestAndResults exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Poms exposing (Score, TestResult, scoreResult, subTotal, total)
import PomsTest


type Model
    = NotStarted
    | Running PomsTest.Model
    | Done (TestResult Score)


init : ( Model, Cmd Msg )
init =
    ( NotStarted
    , Cmd.none
    )


type Msg
    = Start
    | PomsTestMsg PomsTest.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Start, NotStarted ) ->
            let
                ( m, cmds ) =
                    PomsTest.init
            in
            ( Running m, Cmd.map PomsTestMsg cmds )

        ( PomsTestMsg msg_, Running model_ ) ->
            let
                ( m, cmds ) =
                    PomsTest.update msg_ model_

                continue =
                    ( Running m, Cmd.map PomsTestMsg cmds )
            in
            if PomsTest.isDone m then
                case PomsTest.getResult m of
                    Nothing ->
                        -- This is actually a major erro
                        continue

                    Just r ->
                        ( Done r, Cmd.none )

            else
                continue

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        NotStarted ->
            button [ onClick Start ] [ text "Start" ]

        Running model_ ->
            Html.map PomsTestMsg <| PomsTest.view model_

        Done r ->
            let
                sub =
                    subTotal (scoreResult r)

                tmd =
                    total sub
            in
            div []
                [ div [] [ text ("Total Mood Disturbance (TMD): " ++ String.fromInt tmd) ]
                , div [] [ text ("Tension: " ++ String.fromInt sub.tension) ]
                , div [] [ text ("Depression: " ++ String.fromInt sub.depression) ]
                , div [] [ text ("Anger: " ++ String.fromInt sub.anger) ]
                , div [] [ text ("Fatigue: " ++ String.fromInt sub.fatigue) ]
                , div [] [ text ("Confusion: " ++ String.fromInt sub.confusion) ]
                , div [] [ text ("Vigor: " ++ String.fromInt sub.vigour) ]
                , button [ disabled True ] [ text "Submit" ]
                ]
