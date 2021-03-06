module App exposing (Config, Model, Msg, initialModel, sub, update, view)

import CognitoClient
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Login
import PetAndResults
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)
import PomsTestAndResults
import RtServerClient exposing (RtReporter, reportResult)
import RtTest


type PossibleTest
    = RtTest
    | Pet
    | Poms


initialModel : Model
initialModel =
    { rtTestState = RtTest.initialModel
    , petState = PetAndResults.initialModel
    , pomsTestState = PomsTestAndResults.initialModel
    , currentTest = RtTest
    , loginState = NotLoggedIn
    }


type alias Model =
    { rtTestState : RtTest.Model
    , petState : PetAndResults.Model
    , pomsTestState : PomsTestAndResults.Model
    , currentTest : PossibleTest
    , loginState : LoginState
    }


isRunning : Model -> Bool
isRunning { currentTest, petState, rtTestState, pomsTestState } =
    case currentTest of
        RtTest ->
            RtTest.isRunning rtTestState

        Pet ->
            PetAndResults.isRunning petState

        Poms ->
            PomsTestAndResults.isRunning pomsTestState


type LoginState
    = NotLoggedIn
    | LoggingIn Login.Model
    | LoggedIn CognitoClient.TokenSet


type Msg
    = RtTestMsg RtTest.Msg
    | PetTestMsg PetAndResults.Msg
    | PomsTestMsg PomsTestAndResults.Msg
    | SelectTest PossibleTest
    | LoginMsg Login.Msg
    | StartLogin


type alias Config =
    { mkRtTestUpdate : Maybe String -> RtTest.Update
    , mkPomsTestUpdate : Maybe String -> PomsTestAndResults.Update
    , loginUpdate : Login.LoginUpdate
    , loginInit : ( Login.Model, Cmd Login.Msg )
    }


update : Config -> Msg -> Model -> ( Model, Cmd Msg )
update { loginInit, mkRtTestUpdate, mkPomsTestUpdate, loginUpdate } msg ({ rtTestState, petState, loginState, pomsTestState } as s) =
    case ( msg, loginState ) of
        ( StartLogin, NotLoggedIn ) ->
            let
                ( m, cmds ) =
                    loginInit
            in
            ( { s | loginState = LoggingIn m }, Cmd.map LoginMsg cmds )

        ( LoginMsg m, LoggingIn x ) ->
            let
                ( ( next, cmds ), maybeNotifications ) =
                    loginUpdate m x
            in
            case maybeNotifications of
                Nothing ->
                    ( { s | loginState = LoggingIn next }, Cmd.map LoginMsg cmds )

                Just tokens ->
                    ( { s | loginState = LoggedIn tokens }, Cmd.map LoginMsg cmds )

        ( PetTestMsg testMsg, _ ) ->
            let
                ( next, cmds ) =
                    PetAndResults.update testMsg petState
            in
            ( { s | petState = next }, Cmd.map PetTestMsg cmds )

        ( RtTestMsg testMsg, _ ) ->
            let
                mIdToken =
                    case loginState of
                        LoggedIn tokens ->
                            Just tokens.id

                        _ ->
                            Nothing

                ( next, cmds ) =
                    mkRtTestUpdate mIdToken testMsg rtTestState
            in
            ( { s | rtTestState = next }, Cmd.map RtTestMsg cmds )

        ( PomsTestMsg testMsg, _ ) ->
            let
                mIdToken =
                    case loginState of
                        LoggedIn tokens ->
                            Just tokens.id

                        _ ->
                            Nothing

                ( next, cmds ) =
                    mkPomsTestUpdate mIdToken testMsg pomsTestState
            in
            ( { s | pomsTestState = next }, Cmd.map PomsTestMsg cmds )

        ( SelectTest test, _ ) ->
            if isRunning s then
                ( s, Cmd.none )

            else
                ( { s | currentTest = test }, Cmd.none )

        _ ->
            ( s, Cmd.none )


view : Model -> Html Msg
view ({ rtTestState, petState, pomsTestState, loginState, currentTest } as s) =
    let
        ifRunning =
            disabled (isRunning s)

        selectorButtons =
            [ button [ onClick (SelectTest RtTest), ifRunning ] [ text "Reaction Time" ]
            , button [ onClick (SelectTest Pet), ifRunning ] [ text "PET" ]
            , button [ onClick (SelectTest Poms), ifRunning ] [ text "POMS" ]
            ]

        inner loggedIn =
            case currentTest of
                RtTest ->
                    Html.map RtTestMsg (RtTest.view loggedIn rtTestState)

                Pet ->
                    Html.map PetTestMsg (PetAndResults.view petState)

                Poms ->
                    Html.map PomsTestMsg (PomsTestAndResults.view loggedIn pomsTestState)
    in
    div
        []
        (case loginState of
            LoggingIn m ->
                [ Html.map LoginMsg (Login.view m) ]

            NotLoggedIn ->
                [ div []
                    (button [ onClick StartLogin, ifRunning ] [ text "Login" ]
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
