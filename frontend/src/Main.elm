module Main exposing (main)

import App exposing (Model, Msg, initialModel, sub, update, view)
import Browser exposing (element)
import Login
import RtServerClient exposing (reportResult)
import RtTest


main : Program () Model Msg
main =
    element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update =
            update <|
                { mkRtTestUpdate =
                    RtTest.update 80 << Maybe.map (reportResult "http://example.com")
                , loginUpdate = Login.update "XXX"
                }
        , subscriptions = sub
        }
