module Main exposing (main)

import Browser exposing (element)
import Html
import Poms exposing (Score(..))
import PomsButtons exposing (boxes)


main : Program () Score Score
main =
    element
        { init = \_ -> ( ALittle, Cmd.none )
        , view = \m -> Html.map identity (boxes (Just m))
        , update = \m _ -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
