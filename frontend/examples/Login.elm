module Main exposing (main)

import Browser exposing (element)
import CognitoClientMocks exposing (alwaysRequireChangePasswordWithSession, alwaysSucceedChangePasswordWithToken)
import Login exposing (LoginUpdate, Model, Msg, init, initModel, update, view)


main : Program () Model Msg
main =
    element
        { init = \_ -> init
        , view = view
        , update =
            \msg model ->
                Tuple.first <|
                    update
                        { login = alwaysRequireChangePasswordWithSession ""
                        , answerNewPasswordChallenge = alwaysSucceedChangePasswordWithToken ""
                        }
                        msg
                        model
        , subscriptions = \_ -> Sub.none
        }
