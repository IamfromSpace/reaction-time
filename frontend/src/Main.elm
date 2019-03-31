module Main exposing (main)

import Browser exposing (element)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import Json.Decode exposing (field, string, succeed)
import Login
import MultiTest
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)
import RtServerClient exposing (RtError(..), reportResult)
import Time exposing (Posix)


initialModel : Model
initialModel =
    { testState = NotStarted
    , loginState = NotLoggedIn
    }


type alias Model =
    { testState : TestState
    , loginState : LoginState
    }


type TestState
    = NotStarted
    | Running MultiTest.Model
      -- TODO: This data may make sense as a sub Model
    | Done Posix (List (Maybe Float)) SubmitState


type SubmitState
    = NotSubmitted
    | Submitting
    | Submitted
    | SubmitError RtError


type LoginState
    = NotLoggedIn
    | LoggingIn Login.Model
    | LoggedIn String


type Msg
    = TestMsg MultiTest.Msg
    | StartTest
    | LoginMsg Login.Msg
    | StartLogin
    | Submit
    | ReceiveSubmitResult (Maybe RtError)


update : Login.LoginUpdate -> Msg -> Model -> ( Model, Cmd Msg )
update loginUpdate msg ({ testState, loginState } as s) =
    case ( msg, loginState, testState ) of
        ( StartLogin, NotLoggedIn, _ ) ->
            ( { s | loginState = LoggingIn Login.initModel }, Cmd.none )

        ( LoginMsg m, LoggingIn x, _ ) ->
            let
                ( ( next, cmds ), maybeNotifications ) =
                    loginUpdate m x
            in
            case maybeNotifications of
                Nothing ->
                    ( { s | loginState = LoggingIn next }, Cmd.map LoginMsg cmds )

                Just token ->
                    ( { s | loginState = LoggedIn token }, Cmd.none )

        ( StartTest, _, NotStarted ) ->
            ( { s | testState = Running MultiTest.initModel }, Cmd.none )

        ( TestMsg testMsg, _, Running ts ) ->
            let
                { next, notifications, cmds } =
                    MultiTest.update 80 testMsg ts
            in
            case notifications of
                Just ( posix, history ) ->
                    ( { s | testState = Done posix history NotSubmitted }, Cmd.none )

                Nothing ->
                    ( { s | testState = Running next }, Cmd.map TestMsg cmds )

        ( Submit, LoggedIn token, Done posix history submitState ) ->
            let
                ( errorCount, successCount ) =
                    counts history
            in
            ( { s | testState = Done posix history Submitting }
            , Cmd.map ReceiveSubmitResult <|
                reportResult
                    -- TODO: More configurable
                    "https://rtapi.nathanfairhurst.com/api/rt-tester"
                    token
                    { averageSeconds = average history
                    , successCount = successCount
                    , testCount = errorCount + successCount
                    , dateTime = posix
                    }
            )

        ( ReceiveSubmitResult Nothing, _, Done posix history submitState ) ->
            ( { s | testState = Done posix history Submitted }, Cmd.none )

        ( ReceiveSubmitResult (Just e), _, Done posix history submitState ) ->
            ( { s | testState = Done posix history <| SubmitError e }, Cmd.none )

        _ ->
            ( s, Cmd.none )


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


view : Model -> Html Msg
view { testState, loginState } =
    div [ style "display" "flex", style "justify-content" "center", style "align-items" "center", style "height" "90vh" ]
        [ div
            []
          <|
            [ div [ style "margin" "14px", style "font-size" "28px", style "display" "flex", style "justify-content" "center" ] [ text "Reaction Time Tester" ] ]
                ++ (case ( loginState, testState ) of
                        ( LoggingIn m, _ ) ->
                            [ Html.map LoginMsg (Login.view m) ]

                        ( _, NotStarted ) ->
                            [ button [ onClick StartTest ] [ text "Start" ]
                            , button [ onClick StartLogin ] [ text "Login" ]
                            ]

                        ( _, Running ts ) ->
                            [ Html.map TestMsg <| MultiTest.view ts ]

                        ( LoggedIn _, Done _ history submitState ) ->
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

                        ( _, Done _ history submitState ) ->
                            [ viewResult history
                            , button [ onClick StartLogin ] [ text "Login to Submit" ]
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
sub { testState } =
    case testState of
        Running ts ->
            Sub.map TestMsg <| MultiTest.sub ts

        _ ->
            Sub.none


main : Program () Model Msg
main =
    element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update <| Login.update "XXX"
        , subscriptions = sub
        }
