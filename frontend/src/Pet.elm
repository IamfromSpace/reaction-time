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
    ( False, NotStarted )


type RecordState
    = NotStarted
    | Started Posix
    | SomewhatHard Posix
    | SomewhatHard2 Posix Float
    | Hard Posix Float Float
    | Hard2 Posix Float Float Float
    | Done Float Float Float Float



-- Model also holds if we are currently capturing the Posix Time


type alias Model =
    ( Bool, RecordState )


type Msg
    = Record
    | Recorded Posix


toSiDuration : Posix -> Posix -> Float
toSiDuration t1 t0 =
    toFloat (posixToMillis t1 - posixToMillis t0) / 1000


update : Msg -> Model -> ( Model, Cmd Msg )
update msg s =
    case ( msg, s ) of
        ( Record, ( False, _ ) ) ->
            ( ( True, Tuple.second s ), Task.perform Recorded now )

        ( Recorded t, ( True, NotStarted ) ) ->
            ( ( False, Started t ), Cmd.none )

        ( Recorded _, ( True, Started t0 ) ) ->
            ( ( False, SomewhatHard t0 ), Cmd.none )

        ( Recorded t, ( True, SomewhatHard t0 ) ) ->
            ( ( False, SomewhatHard2 t0 (toSiDuration t t0) ), Cmd.none )

        ( Recorded t, ( True, SomewhatHard2 t0 t1 ) ) ->
            ( ( False, Hard t0 t1 (toSiDuration t t0) ), Cmd.none )

        ( Recorded t, ( True, Hard t0 t1 t2 ) ) ->
            ( ( False, Hard2 t0 t1 t2 (toSiDuration t t0) ), Cmd.none )

        ( Recorded t, ( True, Hard2 t0 t1 t2 t3 ) ) ->
            ( ( False, Done t1 t2 t3 (toSiDuration t t0) ), Cmd.none )

        _ ->
            ( s, Cmd.none )



-- A Nothing means there is no next state.


describeIndex : Int -> String
describeIndex i =
    case i of
        0 ->
            "Start"

        1 ->
            "Somewhat Hard"

        3 ->
            "Hard"

        5 ->
            "Very Hard"

        _ ->
            ""


isNothing : Maybe a -> Bool
isNothing m =
    case m of
        Just _ ->
            False

        Nothing ->
            True


view : Model -> Html Msg
view ( _, state ) =
    let
        positionIndex =
            case state of
                NotStarted ->
                    0

                Started _ ->
                    1

                SomewhatHard _ ->
                    2

                SomewhatHard2 _ _ ->
                    3

                Hard _ _ _ ->
                    4

                Hard2 _ _ _ _ ->
                    5

                Done _ _ _ _ ->
                    6
    in
    links
        (100 / 6 / 1.414)
        (100 / 6)
        (List.map
            (\i ->
                if i < positionIndex then
                    LinkConfig (describeIndex i) "#23ba2d" Nothing

                else if i == positionIndex then
                    LinkConfig (describeIndex i) "#baad23" (Just Record)

                else
                    LinkConfig (describeIndex i) "grey" Nothing
            )
            (List.range 0 5)
        )


vw : Float -> String
vw i =
    (\x -> x ++ "vw") <| String.fromFloat i


type alias LinkConfig m =
    { label : String
    , color : String
    , maybeMsg : Maybe m
    }


links : Float -> Float -> List (LinkConfig Msg) -> Html Msg
links diameter length colorsAndMsgs =
    div [ style "position" "relative" ] <|
        Tuple.first <|
            List.foldl
                (\{ color, maybeMsg, label } ( xs, i ) ->
                    ( xs ++ linkPair (i == 0) i diameter length label color maybeMsg
                    , i + 1
                    )
                )
                ( [], 0 )
                colorsAndMsgs


linkPair : Bool -> Int -> Float -> Float -> String -> String -> Maybe Msg -> List (Html Msg)
linkPair final i diameter length labelText color mMsg =
    let
        borderWidth =
            diameter / 15

        circleStyle =
            [ style "background-color" color
            , style "border-width" (vw borderWidth)
            , style "border-color" "white"
            , style "border-style" "solid"
            , style "border-radius" (vw diameter)
            , style "height" (vw (diameter - borderWidth * 2))
            , style "width" (vw (diameter - borderWidth * 2))
            , style "position" "absolute"
            , style "top" (vw 0)
            , style "left" (vw (toFloat i * length))
            ]

        label =
            div
                [ style "width" (vw (length * 0.8))
                , style "position" "absolute"
                , style "text-align" "center"
                , style "font-size" (vw (diameter * 0.25))
                , style "top" (vw (diameter * 1.1))
                , style "left" (vw (((toFloat i - 0.4) * length) + 0.5 * diameter))
                ]
                [ text labelText ]

        lenStyle =
            [ style "background-color" color
            , style "border-width" (vw borderWidth)
            , style "border-color" "white"
            , style "border-style" "solid"
            , style "border-radius" (vw (diameter / 2))
            , style "height" (vw ((diameter - borderWidth * 2) / 3))
            , style "width" (vw (length + diameter / 3))
            , style "position" "absolute"
            , style "top" (vw ((diameter - borderWidth * 2) / 3))
            , style "left" (vw (toFloat (i - 1) * length + (diameter - borderWidth * 2) / 3))
            ]

        ( circle, len ) =
            case mMsg of
                Just msg ->
                    ( div (onClick msg :: circleStyle) [], div (onClick msg :: lenStyle) [] )

                Nothing ->
                    ( div circleStyle [], div lenStyle [] )
    in
    if final then
        [ circle, label ]

    else
        [ len, circle, label ]


main : Program () Model Msg
main =
    element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
