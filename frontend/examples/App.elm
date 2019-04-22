module Main exposing (main)

import App exposing (Model, Msg, initialModel, sub, update, view)
import Browser exposing (element)
import Login
import RtServerClient exposing (reportResult)


main : Program () Model Msg
main =
    element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view

        -- TODO: More Testable
        , update = update <| ( reportResult "http://example.com", Login.update "XXX" )
        , subscriptions = sub
        }
