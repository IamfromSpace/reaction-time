module Main exposing (main)

import App exposing (Model, Msg, initialModel, sub, update, view)
import Browser exposing (element)
import Login
import RtServerClient exposing (successReporter)
import RtTest


main : Program () Model Msg
main =
    element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view

        -- TODO: Still More Testable
        , update =
            update <|
                { mkRtTestUpdate =
                    RtTest.update 4 << Maybe.map (always successReporter)
                , loginUpdate = Login.update "XXX"
                }
        , subscriptions = sub
        }
