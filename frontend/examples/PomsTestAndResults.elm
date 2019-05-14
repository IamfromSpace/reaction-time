module Main exposing (main)

import Browser exposing (element)
import PomsServerClient exposing (successReporter)
import PomsTestAndResults exposing (Model, Msg, init, update, view)


main : Program () Model Msg
main =
    element
        { init = \_ -> init
        , view = view True
        , update = update (Just successReporter)
        , subscriptions = \_ -> Sub.none
        }
