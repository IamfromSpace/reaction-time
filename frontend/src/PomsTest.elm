module PomsTest exposing (Model, Msg, getResult, init, isDone, update, view)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import Poms exposing (Feeling, Score, TestResult, allFeelings, showFeeling)
import PomsButtons exposing (boxes)
import Random exposing (generate)
import Random.List exposing (shuffle)


type alias Model =
    { wip : TestResult (Maybe Score)
    , remainder : List Feeling
    , done : List Feeling
    }


init : ( Model, Cmd Msg )
init =
    ( { wip = Poms.memptyMaybe
      , remainder = []
      , done = []
      }
    , generate GetOrder (shuffle allFeelings)
    )


isDone : Model -> Bool
isDone { remainder } =
    case remainder of
        [] ->
            True

        _ ->
            False


getResult : Model -> Maybe (TestResult Score)
getResult { wip } =
    Poms.sequenceMaybe wip


type Msg
    = GetOrder (List Feeling)
    | Answer Score
    | Back
    | Forward


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        GetOrder list ->
            { model | remainder = list }

        Answer score ->
            case model.remainder of
                [] ->
                    model

                thisFeeling :: newRemainder ->
                    { model
                        | wip = Poms.insert (Just score) thisFeeling model.wip
                        , remainder = newRemainder
                        , done = thisFeeling :: model.done
                    }

        Back ->
            case model.done of
                [] ->
                    model

                lastFeeling :: newDone ->
                    { model
                        | remainder = lastFeeling :: model.remainder
                        , done = newDone
                    }

        Forward ->
            case model.remainder of
                [] ->
                    model

                thisFeeling :: newRemainder ->
                    if Poms.getByFeeling thisFeeling model.wip == Nothing then
                        model

                    else
                        { model
                            | remainder = newRemainder
                            , done = thisFeeling :: model.done
                        }
    , Cmd.none
    )


view : Model -> Html Msg
view { remainder, wip, done } =
    case remainder of
        [] ->
            text "Done!"

        thisFeeling :: _ ->
            div [ style "text-align" "center", style "width" "80vw" ]
                [ div [ style "font-size" "3vw" ] [ text "Today and this week I have felt..." ]
                , div
                    [ style "font-weight" "bold"
                    , style "font-size" "6vw"
                    , style "padding" "2vw"
                    ]
                    [ text (showFeeling thisFeeling) ]
                , Html.map Answer (boxes (Poms.getByFeeling thisFeeling wip))
                , div []
                    [ button
                        [ onClick Back, disabled (List.length done == 0) ]
                        [ text "<" ]
                    , button
                        [ onClick Forward, disabled (Poms.getByFeeling thisFeeling wip == Nothing) ]
                        [ text ">" ]
                    ]
                ]
