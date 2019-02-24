module Login exposing (init, update, view)

import Browser exposing (element)
import CognitoClient exposing (login)
import Html exposing (Html, button, div, form, input, label, text)
import Html.Attributes exposing (disabled, style, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (Error(..))
import Json.Decode exposing (field, string, succeed)
import Platform.Cmd exposing (Cmd)


init : ( Model, Cmd Msg )
init =
    ( { email = "", password = "", loading = False, lastError = Nothing }, Cmd.none )


type alias Model =
    { email : String, password : String, loading : Bool, lastError : Maybe Error }


type Msg
    = UpdateEmail String
    | UpdatePassword String
    | RequestToken
    | ReceiveToken (Result Error String)


update : String -> Msg -> Model -> ( ( Model, Cmd Msg ), Maybe String )
update clientId msg { email, password, loading, lastError } =
    case ( msg, loading ) of
        ( UpdateEmail new, False ) ->
            ( ( { email = new, password = password, loading = loading, lastError = lastError }
              , Cmd.none
              )
            , Nothing
            )

        ( UpdatePassword new, False ) ->
            ( ( { email = email, password = new, loading = loading, lastError = lastError }
              , Cmd.none
              )
            , Nothing
            )

        ( RequestToken, False ) ->
            ( ( { email = email, password = password, loading = True, lastError = Nothing }
              , Cmd.map ReceiveToken <| login clientId email password
              )
            , Nothing
            )

        ( ReceiveToken r, True ) ->
            case r of
                Ok token ->
                    ( ( { email = "", password = "", loading = False, lastError = Nothing }
                      , Cmd.none
                      )
                    , Just token
                    )

                Err e ->
                    ( ( { email = email, password = password, loading = False, lastError = Just e }
                      , Cmd.none
                      )
                    , Nothing
                    )

        ( _, _ ) ->
            ( ( { email = email, password = password, loading = loading, lastError = lastError }
              , Cmd.none
              )
            , Nothing
            )


view : Model -> Html Msg
view { email, password, loading, lastError } =
    form [ onSubmit RequestToken ]
        [ label [] [ text "Email" ]
        , input [ onInput UpdateEmail, type_ "email", value email, disabled loading ] []
        , label [] [ text "Password" ]
        , input [ onInput UpdatePassword, type_ "password", value password, disabled loading ] []
        , button [ disabled loading ] [ text "Login" ]
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


main : Program () Model Msg
main =
    element
        { init = \_ -> init
        , view = view
        , update =
            \msg model ->
                let
                    ( x, _ ) =
                        update "1hc2128v5ffkeokim4uvbpjmj7" msg model
                in
                x
        , subscriptions = \_ -> Sub.none
        }
