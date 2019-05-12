module Main exposing (main)

import Browser exposing (element)
import PomsServerClient exposing (successReporter)
import PomsTestAndResults exposing (Model, Msg, init, update, view)


main : Program () Model Msg
main =
    element
        { init = \_ -> init
        , view = view
        , update = update successReporter
        , subscriptions = \_ -> Sub.none
        }
