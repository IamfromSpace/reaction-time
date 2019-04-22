module Main exposing (main)

import Browser exposing (element)
import PetAndResults exposing (Model, Msg, initialModel, isRunning, update, view)


main : Program () Model Msg
main =
    element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
