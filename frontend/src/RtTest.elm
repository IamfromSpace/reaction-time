module RtTest exposing (Model, Msg, Update, initialModel, isRunning, sub, update, view)

import Browser exposing (element)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import MultiTest
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)
import RtServerClient exposing (RtError(..), RtReporter)
import Time exposing (Posix)


initialModel : Model
initialModel =
    NotStarted


type Model
    = NotStarted
    | Running MultiTest.Model
      -- TODO: This data may make sense as a sub Model
    | Done Posix (List (Maybe Float)) SubmitState


isRunning : Model -> Bool
isRunning model =
    case model of
        Running _ ->
            True

        _ ->
            False


type SubmitState
    = NotSubmitted
    | Submitting
    | Submitted
    | SubmitError RtError


type Msg
    = TestMsg MultiTest.Msg
    | StartTest
    | Submit
    | ReceiveSubmitResult (Maybe RtError)


type alias Update =
    Msg -> Model -> ( Model, Cmd Msg )


update : Int -> Maybe RtReporter -> Update
update runs mReportResult msg testState =
    case ( msg, mReportResult, testState ) of
        ( StartTest, _, NotStarted ) ->
            ( Running MultiTest.initModel, Cmd.none )

        ( TestMsg testMsg, _, Running ts ) ->
            let
                { next, notifications, cmds } =
                    MultiTest.update runs testMsg ts
            in
            case notifications of
                Just ( posix, history ) ->
                    ( Done posix history NotSubmitted, Cmd.none )

                Nothing ->
                    ( Running next, Cmd.map TestMsg cmds )

        ( Submit, Just reportResult, Done posix history submitState ) ->
            let
                ( errorCount, successCount ) =
                    counts history
            in
            ( Done posix history Submitting
            , Cmd.map ReceiveSubmitResult <|
                reportResult
                    { averageSeconds = average history
                    , successCount = successCount
                    , testCount = errorCount + successCount
                    , dateTime = posix
                    }
            )

        ( ReceiveSubmitResult Nothing, _, Done posix history submitState ) ->
            ( Done posix history Submitted, Cmd.none )

        ( ReceiveSubmitResult (Just e), _, Done posix history submitState ) ->
            ( Done posix history <| SubmitError e, Cmd.none )

        _ ->
            ( testState, Cmd.none )


average : List (Maybe Float) -> Float
average xs =
    let
        filtered =
            List.filterMap identity xs
    in
    filtered
        |> List.foldr (+) 0
        |> (\x -> x / toFloat (List.length filtered))


counts : List (Maybe Float) -> ( Int, Int )
counts =
    List.partition ((==) Nothing) >> (\( a, b ) -> ( List.length a, List.length b ))


view : Bool -> Model -> Html Msg
view loggedIn testState =
    div [ style "display" "flex", style "justify-content" "center", style "align-items" "center", style "height" "90vh" ]
        [ div
            []
          <|
            [ div [ style "margin" "14px", style "font-size" "28px", style "display" "flex", style "justify-content" "center" ] [ text "Reaction Time Tester" ] ]
                ++ (case ( loggedIn, testState ) of
                        ( _, NotStarted ) ->
                            [ button [ onClick StartTest ] [ text "Start" ]
                            ]

                        ( _, Running ts ) ->
                            [ Html.map TestMsg <| MultiTest.view ts ]

                        ( True, Done _ history submitState ) ->
                            [ viewResult history
                            , button
                                [ onClick Submit
                                , disabled (submitState == Submitting || submitState == Submitted || submitState == SubmitError Misconfiguration)
                                ]
                                [ text <|
                                    if submitState == Submitted then
                                        "Done!"

                                    else
                                        "Submit"
                                ]
                            , text <|
                                case submitState of
                                    SubmitError MaybeRetry ->
                                        "A network error occurred; retries may or may not help."

                                    SubmitError RetryLater ->
                                        "Please retry later."

                                    SubmitError Misconfiguration ->
                                        "An unexpected error occured; retries will not help."

                                    _ ->
                                        ""
                            ]

                        ( False, Done _ history submitState ) ->
                            [ viewResult history
                            , button [ disabled True ] [ text "Must be logged in to Submit" ]
                            ]
                   )
        ]


viewResult : List (Maybe Float) -> Html a
viewResult history =
    div
        [ style "display" "flex"
        , style "justify-content" "space-between"
        , style "flex-direction" "row"
        ]
        [ div [] [ text <| (pastDecimal 1 (String.fromFloat (average history)) ++ "ms") ]
        , div [] [ text <| (\( e, c ) -> String.fromInt c ++ "/" ++ String.fromInt (c + e)) <| counts history ]
        ]



-- Drop anything n characters after a period
-- Note that for something like `pastDecimal 1 "3.0000000002"`
-- this will not return "3"


pastDecimal : Int -> String -> String
pastDecimal n s =
    let
        go built str =
            case str of
                '.' :: t ->
                    List.reverse built ++ '.' :: List.take n t

                h :: t ->
                    go (h :: built) t

                [] ->
                    List.reverse built
    in
    String.fromList <| go [] <| String.toList s


sub : Model -> Sub Msg
sub testState =
    case testState of
        Running ts ->
            Sub.map TestMsg <| MultiTest.sub ts

        _ ->
            Sub.none
