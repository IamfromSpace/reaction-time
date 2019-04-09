module Main exposing (main)

import Browser exposing (element)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Login
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)
import RtServerClient exposing (RtReporter, reportResult)
import RtTest


initialModel : Model
initialModel =
    { testState = RtTest.initialModel
    , loginState = NotLoggedIn
    }


type alias Model =
    { testState : RtTest.Model
    , loginState : LoginState
    }


type LoginState
    = NotLoggedIn
    | LoggingIn Login.Model
    | LoggedIn String


type Msg
    = RtTestMsg RtTest.Msg
    | LoginMsg Login.Msg
    | StartLogin


update : ( String -> RtReporter, Login.LoginUpdate ) -> Msg -> Model -> ( Model, Cmd Msg )
update ( mkReporter, loginUpdate ) msg ({ testState, loginState } as s) =
    case ( msg, loginState ) of
        ( StartLogin, NotLoggedIn ) ->
            ( { s | loginState = LoggingIn Login.initModel }, Cmd.none )

        ( LoginMsg m, LoggingIn x ) ->
            let
                ( ( next, cmds ), maybeNotifications ) =
                    loginUpdate m x
            in
            case maybeNotifications of
                Nothing ->
                    ( { s | loginState = LoggingIn next }, Cmd.map LoginMsg cmds )

                Just token ->
                    ( { s | loginState = LoggedIn token }, Cmd.none )

        ( RtTestMsg testMsg, _ ) ->
            let
                updater =
                    case loginState of
                        LoggedIn token ->
                            RtTest.update <| Just <| mkReporter token

                        _ ->
                            RtTest.update Nothing

                ( next, cmds ) =
                    updater testMsg testState
            in
            ( { s | testState = next }, Cmd.map RtTestMsg cmds )

        _ ->
            ( s, Cmd.none )


view : Model -> Html Msg
view { testState, loginState } =
    div
        []
        (case loginState of
            LoggingIn m ->
                [ Html.map LoginMsg (Login.view m) ]

            NotLoggedIn ->
                [ button [ onClick StartLogin ] [ text "Login" ]
                , Html.map RtTestMsg (RtTest.view False testState)
                ]

            LoggedIn _ ->
                [ button [ disabled True ] [ text "Logout" ]
                , Html.map RtTestMsg (RtTest.view True testState)
                ]
        )


sub : Model -> Sub Msg
sub { testState } =
    Sub.map RtTestMsg <| RtTest.sub testState


main : Program () Model Msg
main =
    element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update <| ( reportResult "http://example.com", Login.update "XXX" )
        , subscriptions = sub
        }
