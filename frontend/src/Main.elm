module Main exposing (main)

import Browser exposing (element)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Login
import PetAndResults
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)
import RtServerClient exposing (RtReporter, reportResult)
import RtTest


type PossibleTest
    = RtTest
    | Pet


initialModel : Model
initialModel =
    { rtTestState = RtTest.initialModel
    , petState = PetAndResults.initialModel
    , currentTest = RtTest
    , loginState = NotLoggedIn
    }


type alias Model =
    { rtTestState : RtTest.Model
    , petState : PetAndResults.Model
    , currentTest : PossibleTest
    , loginState : LoginState
    }


type LoginState
    = NotLoggedIn
    | LoggingIn Login.Model
    | LoggedIn String


type Msg
    = RtTestMsg RtTest.Msg
    | PetTestMsg PetAndResults.Msg
    | SelectTest PossibleTest
    | LoginMsg Login.Msg
    | StartLogin


update : ( String -> RtReporter, Login.LoginUpdate ) -> Msg -> Model -> ( Model, Cmd Msg )
update ( mkReporter, loginUpdate ) msg ({ rtTestState, petState, loginState } as s) =
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

        ( PetTestMsg testMsg, _ ) ->
            let
                ( next, cmds ) =
                    PetAndResults.update testMsg petState
            in
            ( { s | petState = next }, Cmd.map PetTestMsg cmds )

        ( RtTestMsg testMsg, _ ) ->
            let
                updater =
                    case loginState of
                        LoggedIn token ->
                            RtTest.update <| Just <| mkReporter token

                        _ ->
                            RtTest.update Nothing

                ( next, cmds ) =
                    updater testMsg rtTestState
            in
            ( { s | rtTestState = next }, Cmd.map RtTestMsg cmds )

        ( SelectTest test, _ ) ->
            ( { s | currentTest = test }, Cmd.none )

        _ ->
            ( s, Cmd.none )


view : Model -> Html Msg
view { rtTestState, petState, loginState, currentTest } =
    let
        selectorButtons =
            [ button [ onClick (SelectTest RtTest) ] [ text "Reaction Time" ]
            , button [ onClick (SelectTest Pet) ] [ text "PET" ]
            ]

        inner loggedIn =
            case currentTest of
                RtTest ->
                    Html.map RtTestMsg (RtTest.view loggedIn rtTestState)

                Pet ->
                    Html.map PetTestMsg (PetAndResults.view petState)
    in
    div
        []
        (case loginState of
            LoggingIn m ->
                [ Html.map LoginMsg (Login.view m) ]

            NotLoggedIn ->
                [ div []
                    (button [ onClick StartLogin ] [ text "Login" ]
                        :: selectorButtons
                    )
                , inner False
                ]

            LoggedIn _ ->
                [ div []
                    (button [ disabled True ] [ text "Logout" ]
                        :: selectorButtons
                    )
                , inner True
                ]
        )


sub : Model -> Sub Msg
sub { rtTestState } =
    Sub.map RtTestMsg <| RtTest.sub rtTestState


main : Program () Model Msg
main =
    element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update <| ( reportResult "http://example.com", Login.update "XXX" )
        , subscriptions = sub
        }
