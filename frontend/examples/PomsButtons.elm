module Main exposing (main)

import Browser exposing (element)
import Html
import Poms exposing (Score)
import PomsButtons exposing (boxes)


main : Program () () Score
main =
    element
        { init = \_ -> ( (), Cmd.none )
        , view = \_ -> Html.map (Debug.log "clicked!") boxes
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
