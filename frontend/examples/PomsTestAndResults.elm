module Main exposing (main)

import Browser exposing (element)
import PomsTestAndResults exposing (Model, Msg, init, update, view)


main : Program () Model Msg
main =
    element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
