module Main exposing (main)

import Browser exposing (element)
import RtServerClient
import RtTest exposing (Model, Msg, initialModel, isRunning, sub, update, view)


main : Program () Model Msg
main =
    element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view True
        , update = update <| Just RtServerClient.successReporter
        , subscriptions = sub
        }
