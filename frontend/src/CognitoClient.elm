module CognitoClient exposing (login)

import Http exposing (Error, expectJson, header, request, stringBody)
import Json.Decode as D
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


login : String -> String -> String -> Cmd (Result Error String)
login clientId username password =
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

        -- For now we just want the IdToken
        , expect = expectJson identity (D.at [ "AuthenticationResult", "IdToken" ] D.string)
        , timeout = Just 5000
        , tracker = Nothing
        }
