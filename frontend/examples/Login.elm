module Main exposing (main)

import Browser exposing (element)
import Login exposing (LoginUpdate, Model, Msg, init, initModel, update, view)


main : Program () Model Msg
main =
    element
        { init = \_ -> init
        , view = view
        , update =
            \msg model ->
                let
                    ( x, _ ) =
                        update "XXX" msg model
                in
                x
        , subscriptions = \_ -> Sub.none
        }
