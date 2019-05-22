module Main exposing (main)

import Browser exposing (element)
import CognitoClient exposing (LoginResult(..), TokenSet)
import CognitoClientMocks exposing (alwaysRequireChangePasswordWithSession, alwaysSucceedChangePasswordWithTokens, alwaysSucceedRefreshWithTokens)
import Login exposing (Config, LoginUpdate, Model, Msg, mkInit, update, view)
import Task


tokenSet : TokenSet
tokenSet =
    { id = "id"
    , refresh = "refresh"
    , access = "session"
    }


config : Config
config =
    { login = alwaysRequireChangePasswordWithSession ""
    , answerNewPasswordChallenge =
        alwaysSucceedChangePasswordWithTokens tokenSet
    , refreshSession =
        alwaysSucceedRefreshWithTokens tokenSet
    , cacheRefreshToken =
        \t -> Task.perform (always ()) (Task.succeed (Debug.log "saving token" t))
    }


main : Program () Model Msg
main =
    element
        { init = \_ -> mkInit config (Just "refresh")
        , view = view
        , update =
            \msg model ->
                case update config msg model of
                    ( x, Just ts ) ->
                        always x <| Debug.log "Got tokens" ts

                    ( x, _ ) ->
                        x
        , subscriptions = \_ -> Sub.none
        }
