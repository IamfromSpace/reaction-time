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
    let
        rtEndpoint =
            "http://example.com/api/tests/rt"

        pomsEndpoint =
            "http://example.com/api/tests/poms"

        cognitoClientId =
            "XXX"
    in
    element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update =
            update <|
                { mkRtTestUpdate =
                    RtTest.update 80 << Maybe.map (RtServerClient.reportResult rtEndpoint)
                , mkPomsTestUpdate =
                    PomsTestAndResults.update << Maybe.map (PomsServerClient.reportResult pomsEndpoint)
                , loginUpdate = Login.update (mkLoginUpdateConfig cognitoClientId)
                }
        , subscriptions = sub
        }
