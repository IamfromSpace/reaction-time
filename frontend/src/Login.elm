module Login exposing (LoginUpdate, Model, Msg, init, initModel, update, view)

import CognitoClient exposing (LoginResult(..), answerNewPasswordChallenge, login)
import Html exposing (Html, button, div, form, input, label, text)
import Html.Attributes exposing (disabled, style, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (Error(..))
import Json.Decode exposing (field, string, succeed)
import Platform.Cmd exposing (Cmd)


initModel : Model
initModel =
    { email = "", password = "", session = Nothing, loading = False, lastError = Nothing }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


type alias Model =
    { email : String, password : String, session : Maybe String, loading : Bool, lastError : Maybe Error }


type Msg
    = UpdateEmail String
    | UpdatePassword String
    | RequestToken
    | RequestNewPassword String
    | ReceiveError Error
    | ReceiveToken String
    | ReceiveChallenge String


loginResponseToMsg : Result Error CognitoClient.LoginResult -> Msg
loginResponseToMsg result =
    case result of
        Ok (LoggedIn token) ->
            ReceiveToken token

        Ok (NewPasswordRequired session) ->
            ReceiveChallenge session

        Err e ->
            ReceiveError e


challengeResponseToMsg : Result Error String -> Msg
challengeResponseToMsg result =
    case result of
        Ok token ->
            ReceiveToken token

        Err e ->
            ReceiveError e


type alias LoginUpdate =
    Msg -> Model -> ( ( Model, Cmd Msg ), Maybe String )


update : String -> LoginUpdate
update clientId msg ({ email, password, loading, lastError } as s) =
    case ( msg, loading ) of
        ( UpdateEmail new, False ) ->
            ( ( { s | email = new }, Cmd.none ), Nothing )

        ( UpdatePassword new, False ) ->
            ( ( { s | password = new }, Cmd.none ), Nothing )

        ( RequestToken, False ) ->
            ( ( { s | loading = True, lastError = Nothing }
              , Cmd.map loginResponseToMsg <| login clientId email password
              )
            , Nothing
            )

        ( RequestNewPassword session, False ) ->
            ( ( { s | loading = True, lastError = Nothing }
              , Cmd.map challengeResponseToMsg <| answerNewPasswordChallenge session clientId email password
              )
            , Nothing
            )

        ( ReceiveToken token, True ) ->
            ( ( { email = ""
                , password = ""
                , loading = False
                , lastError = Nothing
                , session = Nothing
                }
              , Cmd.none
              )
            , Just token
            )

        ( ReceiveChallenge session, True ) ->
            ( ( { s | password = "", loading = False, lastError = Nothing, session = Just session }
              , Cmd.none
              )
            , Nothing
            )

        ( ReceiveError e, True ) ->
            ( ( { s | loading = False, lastError = Just e }, Cmd.none ), Nothing )

        ( _, _ ) ->
            ( ( s, Cmd.none ), Nothing )


view : Model -> Html Msg
view { email, password, loading, lastError, session } =
    form
        [ onSubmit <|
            case session of
                Nothing ->
                    RequestToken

                Just s ->
                    RequestNewPassword s
        ]
        [ label [] [ text "Email" ]
        , input [ onInput UpdateEmail, type_ "email", value email, disabled loading ] []
        , label [] [ text "Password" ]
        , input [ onInput UpdatePassword, type_ "password", value password, disabled loading ] []
        , button [ disabled loading ]
            [ text <|
                case session of
                    Nothing ->
                        "Login"

                    Just _ ->
                        "Update Password"
            ]
        , text <|
            case lastError of
                Nothing ->
                    ""

                Just (BadStatus x) ->
                    "Unexpected Status " ++ String.fromInt x

                Just NetworkError ->
                    "Is your internet connected?"

                Just Timeout ->
                    "Timeout trying to fetch token"

                Just (BadBody _) ->
                    "Unexpected response from auth service"

                Just (BadUrl _) ->
                    "Tried to reach an unreachable endpoint"
        ]
