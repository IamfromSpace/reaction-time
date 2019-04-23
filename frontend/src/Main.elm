module Main exposing (main)

import App exposing (Model, Msg, initialModel, sub, update, view)
import Browser exposing (element)
import CognitoClient exposing (mkAnswerNewPasswordChallenge, mkLogin)
import Login
import RtServerClient exposing (reportResult)
import RtTest


mkLoginUpdateConfig : String -> Login.Config
mkLoginUpdateConfig clientId =
    { login = mkLogin clientId
    , answerNewPasswordChallenge = mkAnswerNewPasswordChallenge clientId
    }


main : Program () Model Msg
main =
    element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update =
            update <|
                { mkRtTestUpdate =
                    RtTest.update 80 << Maybe.map (reportResult "http://example.com")
                , loginUpdate = Login.update (mkLoginUpdateConfig "XXX")
                }
        , subscriptions = sub
        }
