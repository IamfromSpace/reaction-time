module PomsTestAndResults exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Poms exposing (Score, SubTotal, TestResult, scoreResult, subTotal, total)
import PomsServerClient exposing (PomsError(..), PomsReporter, PomsResult)
import PomsTest
import Task
import Time exposing (Posix, now)


type Model
    = NotStarted
    | Running PomsTest.Model
    | DoneAwaitingTime (TestResult Score)
    | Done (TestResult Score) Posix SubmitState


type SubmitState
    = NotSubmitted
    | Submitting
    | Submitted
    | SubmitError PomsError


init : ( Model, Cmd Msg )
init =
    ( NotStarted
    , Cmd.none
    )


type Msg
    = Start
    | PomsTestMsg PomsTest.Msg
    | Now Posix
    | Submit
    | SubmitResult (Maybe PomsError)


update : PomsReporter -> Msg -> Model -> ( Model, Cmd Msg )
update reportResult msg model =
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
                        ( DoneAwaitingTime r, Task.perform Now now )

            else
                continue

        ( Now posix, DoneAwaitingTime r ) ->
            ( Done r posix NotSubmitted, Cmd.none )

        ( Submit, Done r t NotSubmitted ) ->
            doSubmit reportResult r t

        ( Submit, Done r t (SubmitError MaybeRetry) ) ->
            doSubmit reportResult r t

        ( Submit, Done r t (SubmitError RetryLater) ) ->
            doSubmit reportResult r t

        ( SubmitResult x, Done r t Submitting ) ->
            case x of
                Nothing ->
                    ( Done r t Submitted, Cmd.none )

                Just e ->
                    ( Done r t (SubmitError e), Cmd.none )

        _ ->
            ( model, Cmd.none )


doSubmit : PomsReporter -> TestResult Score -> Posix -> ( Model, Cmd Msg )
doSubmit reportResult r t =
    let
        sub =
            subTotal (scoreResult r)

        v =
            { tmd = total sub
            , subTotal = sub
            , dateTime = t
            }
    in
    ( Done r t Submitting, Cmd.map SubmitResult (reportResult v) )


view : Model -> Html Msg
view model =
    case model of
        NotStarted ->
            button [ onClick Start ] [ text "Start" ]

        Running model_ ->
            Html.map PomsTestMsg <| PomsTest.view model_

        DoneAwaitingTime r ->
            doneView r Nothing

        Done r _ submitState ->
            doneView r (Just submitState)


doneView : TestResult Score -> Maybe SubmitState -> Html Msg
doneView r mSubmitState =
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
        , div [] [ text ("Vigor: " ++ String.fromInt sub.vigor) ]
        , submitView mSubmitState
        ]


submitView : Maybe SubmitState -> Html Msg
submitView mSubmitState =
    case mSubmitState of
        Nothing ->
            button [ disabled True ] [ text "Submit" ]

        Just NotSubmitted ->
            button [ onClick Submit ] [ text "Submit" ]

        Just Submitting ->
            button [ disabled True ] [ text "Submitting" ]

        Just Submitted ->
            button [ disabled True ] [ text "Submitted" ]

        Just (SubmitError MaybeRetry) ->
            div []
                [ button [ onClick Submit ] [ text "Submit" ]
                , text "A network error occurred; retries may or may not help."
                ]

        Just (SubmitError RetryLater) ->
            div []
                [ button [ onClick Submit ] [ text "Submit" ]
                , text "Please retry later."
                ]

        Just (SubmitError Misconfiguration) ->
            text "An unexpected error occured; retries will not help."
