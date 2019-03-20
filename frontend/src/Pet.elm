module Main exposing (main)

import Browser exposing (element)
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import Platform.Cmd exposing (Cmd)
import Task exposing (perform)
import Time exposing (Posix, now, posixToMillis)


initialModel : Model
initialModel =
    { recordings = Dict.empty
    , startTime = Nothing
    }


type alias Model =
    { recordings : Dict Int Float
    , startTime : Maybe Posix
    }


type Msg
    = Start
    | Started Posix
    | Record Int
    | Recorded Int Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ startTime, recordings } as s) =
    case ( msg, startTime ) of
        ( Start, Nothing ) ->
            ( s, Task.perform Started now )

        ( Started t, Nothing ) ->
            ( { s | startTime = Just t }, Cmd.none )

        ( Record x, Just _ ) ->
            ( s, Task.perform (Recorded x) now )

        ( Recorded x t, Just st ) ->
            let
                seconds =
                    toFloat (posixToMillis t - posixToMillis st) / 1000
            in
            ( { s | recordings = Dict.insert x seconds recordings }, Cmd.none )

        _ ->
            ( s, Cmd.none )


describeRpe : Int -> Maybe String
describeRpe i =
    case i of
        6 ->
            Just "No exertion at all"

        7 ->
            Just "Extremely light"

        8 ->
            Just "Between Extremely and Very Light"

        9 ->
            Just "Very Light"

        10 ->
            Just "Between Very Light and Light"

        11 ->
            Just "Light"

        12 ->
            Just "Between Light and Somewhat Hard"

        13 ->
            Just "Somewhat Hard"

        14 ->
            Just "Between Somewhat Hard and Hard"

        15 ->
            Just "Hard"

        16 ->
            Just "Between Hard and Very Hard"

        17 ->
            Just "Very Hard"

        18 ->
            Just "Between Very Hard and Extremely Hard"

        19 ->
            Just "Extremely Hard"

        20 ->
            Just "Maximal exertion"

        _ ->
            Nothing


last : List a -> Maybe a
last list =
    case list of
        [] ->
            Nothing

        [ x ] ->
            Just x

        h :: t ->
            last t


view : Model -> Html Msg
view { startTime, recordings } =
    let
        nextRpe =
            Maybe.withDefault 12 (last (Dict.keys recordings)) + 1
    in
    div []
        [ case startTime of
            Just _ ->
                button [ onClick (Record nextRpe) ] [ text <| Maybe.withDefault "TODO!" <| describeRpe nextRpe ]

            Nothing ->
                button [ onClick Start ] [ text "Start" ]
        ]


main : Program () Model Msg
main =
    element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
