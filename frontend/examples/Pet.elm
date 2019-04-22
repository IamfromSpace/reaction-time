module Main exposing (main)

import Browser exposing (element)
import Pet exposing (Model, Msg, initialModel, update, view)


main : Program () Model Msg
main =
    element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = \x y -> Tuple.second <| update x y
        , subscriptions = \_ -> Sub.none
        }
