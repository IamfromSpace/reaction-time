module Main exposing (main)

import App exposing (Model, Msg, initialModel, sub, update, view)
import Browser exposing (element)
import CognitoClientMocks exposing (alwaysRequireChangePasswordWithSession, alwaysSucceedChangePasswordWithToken)
import Login
import PomsServerClient
import PomsTestAndResults
import RtServerClient
import RtTest


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
                , loginUpdate =
                    Login.update
                        { login = alwaysRequireChangePasswordWithSession ""
                        , answerNewPasswordChallenge = alwaysSucceedChangePasswordWithToken ""
                        }
                }
        , subscriptions = sub
        }
