module Main exposing (main)

import App exposing (Model, Msg, initialModel, sub, update, view)
import Browser exposing (element)
import CognitoClient exposing (TokenSet)
import CognitoClientMocks exposing (alwaysRequireChangePasswordWithSession, alwaysSucceedChangePasswordWithTokens, alwaysSucceedRefreshWithTokens)
import Login
import PomsServerClient
import PomsTestAndResults
import RtServerClient
import RtTest
import Task


tokenSet : TokenSet
tokenSet =
    { id = "id"
    , refresh = "refresh"
    , access = "access"
    }


loginConfig : Login.Config
loginConfig =
    { login = alwaysRequireChangePasswordWithSession ""
    , answerNewPasswordChallenge = alwaysSucceedChangePasswordWithTokens tokenSet
    , refreshSession =
        alwaysSucceedRefreshWithTokens tokenSet
    , cacheRefreshToken =
        \t -> Task.perform (always ()) (Task.succeed (Debug.log "saving token" t))
    }


main : Program () Model Msg
main =
    element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update =
            update <|
                { mkRtTestUpdate =
                    RtTest.update 4 << Maybe.map (always RtServerClient.successReporter)
                , mkPomsTestUpdate =
                    PomsTestAndResults.update << Maybe.map (always PomsServerClient.successReporter)
                , loginUpdate = Login.update loginConfig
                , loginInit = Login.mkInit loginConfig Nothing
                }
        , subscriptions = sub
        }
