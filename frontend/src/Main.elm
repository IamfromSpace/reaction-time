module Main exposing (main)

import App exposing (Model, Msg, initialModel, sub, update, view)
import Browser exposing (element)
import CognitoClient exposing (mkAnswerNewPasswordChallenge, mkLogin)
import Login
import PomsServerClient
import PomsTestAndResults
import RtServerClient
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
                    RtTest.update 80 << Maybe.map (RtServerClient.reportResult "http://example.com/tests/rt")
                , mkPomsTestUpdate =
                    PomsTestAndResults.update << Maybe.map (PomsServerClient.reportResult "http://example.com/tests/poms")
                , loginUpdate = Login.update (mkLoginUpdateConfig "XXX")
                }
        , subscriptions = sub
        }
