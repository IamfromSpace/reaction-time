port module Main exposing (main)

import App exposing (Model, Msg, initialModel, sub, update, view)
import Browser exposing (element)
import CognitoClient exposing (mkAnswerNewPasswordChallenge, mkLogin, mkRefreshSession)
import Login
import PomsServerClient
import PomsTestAndResults
import RtServerClient
import RtTest
import Utils


port cacheRefreshToken : String -> Cmd msg


mkLoginUpdateConfig : String -> String -> Login.Config
mkLoginUpdateConfig region clientId =
    { login = mkLogin region clientId
    , answerNewPasswordChallenge = mkAnswerNewPasswordChallenge region clientId
    , cacheRefreshToken = cacheRefreshToken
    , refreshSession = mkRefreshSession region clientId
    }


main : Program (Maybe String) ( App.Config, Model ) Msg
main =
    let
        rtEndpoint =
            "http://example.com/api/tests/rt"

        pomsEndpoint =
            "http://example.com/api/tests/poms"

        cognitoClientId =
            "XXX"

        cognitoRegion =
            "us-west-2"
    in
    (element << Utils.configStyle)
        { init =
            \mRefreshToken ->
                ( { mkRtTestUpdate =
                        RtTest.update 80 << Maybe.map (RtServerClient.reportResult rtEndpoint)
                  , mkPomsTestUpdate =
                        PomsTestAndResults.update << Maybe.map (PomsServerClient.reportResult pomsEndpoint)
                  , loginUpdate = Login.update (mkLoginUpdateConfig cognitoRegion cognitoClientId)
                  , loginInit = Login.mkInit (mkLoginUpdateConfig cognitoRegion cognitoClientId) refreshToken
                  }
                , ( initialModel, Cmd.none )
                )
        , view = view
        , update = update
        , subscriptions = sub
        }
