module PomsTest exposing (Model, Msg, getResult, init, isDone, update, view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Poms exposing (Feeling, Score, TestResult, allFeelings, showFeeling)
import PomsButtons exposing (boxes)
import Random exposing (generate)
import Random.List exposing (shuffle)


type alias Model =
    { wip : TestResult (Maybe Score)
    , remainder : List Feeling
    }


init : ( Model, Cmd Msg )
init =
    ( { wip = Poms.memptyMaybe
      , remainder = []
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
                    }
    , Cmd.none
    )


view : Model -> Html Msg
view { remainder } =
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
                , Html.map Answer boxes
                ]
