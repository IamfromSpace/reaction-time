module CognitoClient exposing (AnswerNewPasswordChallenge, Login, LoginResult(..), mkAnswerNewPasswordChallenge, mkLogin)

import Http exposing (Error, expectJson, header, request, stringBody)
import Json.Decode as D exposing (Decoder)
import Json.Encode exposing (Value, encode, object, string)


type alias LoginInfo =
    { username : String
    , password : String
    }


loginInfoToValue : LoginInfo -> Value
loginInfoToValue { username, password } =
    object [ ( "USERNAME", string username ), ( "PASSWORD", string password ) ]


type alias LoginBody =
    { authFlow : String
    , clientId : String
    , authParameters : LoginInfo
    }


loginBodyToValue : LoginBody -> Value
loginBodyToValue { authFlow, clientId, authParameters } =
    object
        [ ( "AuthFlow", string authFlow )
        , ( "ClientId", string clientId )
        , ( "AuthParameters", loginInfoToValue authParameters )
        ]


type
    LoginResult
    -- For now we just want the IdToken
    = LoggedIn String
    | NewPasswordRequired String


decodeToken : Decoder String
decodeToken =
    D.at [ "AuthenticationResult", "IdToken" ] D.string


decodeLoginResponse : Decoder LoginResult
decodeLoginResponse =
    D.oneOf
        [ D.map LoggedIn <| decodeToken
        , D.field "ChallengeName" D.string
            |> D.andThen
                (\challengeName ->
                    if challengeName == "NEW_PASSWORD_REQUIRED" then
                        D.map NewPasswordRequired <| D.field "Session" D.string

                    else
                        D.fail "Only the new password challenge is supported!"
                )
        ]


type alias Login =
    String -> String -> Cmd (Result Error LoginResult)


mkLogin : String -> Login
mkLogin clientId username password =
    request
        { method = "POST"
        , headers =
            [ header "X-Amz-Target" "AWSCognitoIdentityProviderService.InitiateAuth" ]
        , url = "https://cognito-idp.us-east-1.amazonaws.com"
        , body =
            stringBody "application/x-amz-json-1.1" <|
                encode 0 <|
                    loginBodyToValue
                        { authFlow = "USER_PASSWORD_AUTH"
                        , clientId = clientId
                        , authParameters = { username = username, password = password }
                        }
        , expect = expectJson identity decodeLoginResponse
        , timeout = Just 5000
        , tracker = Nothing
        }


type alias AnswerNewPasswordChallenge =
    String -> String -> String -> Cmd (Result Error String)


mkAnswerNewPasswordChallenge : String -> AnswerNewPasswordChallenge
mkAnswerNewPasswordChallenge clientId session username password =
    request
        { method = "POST"
        , headers =
            [ header "X-Amz-Target" "AWSCognitoIdentityProviderService.RespondToAuthChallenge" ]
        , url = "https://cognito-idp.us-east-1.amazonaws.com"
        , body =
            stringBody "application/x-amz-json-1.1" <|
                encode 0 <|
                    object
                        [ ( "ChallengeName", string "NEW_PASSWORD_REQUIRED" )
                        , ( "ClientId", string clientId )
                        , ( "ChallengeResponses"
                          , object
                                [ ( "USERNAME", string username )
                                , ( "NEW_PASSWORD", string password )
                                ]
                          )
                        , ( "Session", string session )
                        ]
        , expect = expectJson identity decodeToken
        , timeout = Just 5000
        , tracker = Nothing
        }
