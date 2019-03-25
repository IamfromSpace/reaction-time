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


describeNext : RecordState -> Maybe String
describeNext next =
    case next of
        NotStarted ->
            Just "Start"

        Started _ ->
            Just "Somewhat Hard"

        SomewhatHard _ ->
            Just "Between Somewhat Hard and Hard"

        SomewhatHard2 _ _ ->
            Just "Hard"

        Hard _ _ _ ->
            Just "Between Hard and Very Hard"

        Hard2 _ _ _ _ ->
            Just "Very Hard"

        Done _ _ _ _ ->
            Nothing


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
        description =
            describeNext state
    in
    div []
        [ button
            [ onClick Record
            , disabled (isNothing description)
            ]
            [ text <| Maybe.withDefault "Done!" <| description ]
        , links 50 100 [ ( "red", Record ), ( "blue", Record ), ( "blue", Record ) ]
        ]


px : Float -> String
px i =
    (\x -> x ++ "px") <| String.fromFloat i


links : Float -> Float -> List ( String, Msg ) -> Html Msg
links diameter length colorsAndMsgs =
    div [ style "position" "relative" ] <|
        Tuple.first <|
            List.foldl
                (\( color, msg ) ( xs, i ) ->
                    ( xs ++ linkPair (i == (List.length xs - 2)) i diameter length color msg
                    , i + 1
                    )
                )
                ( [], 0 )
                colorsAndMsgs


linkPair : Bool -> Int -> Float -> Float -> String -> Msg -> List (Html Msg)
linkPair final i diameter length color msg =
    let
        borderWidth =
            diameter / 15

        circle =
            div
                [ style "background-color" color
                , style "border-width" (px borderWidth)
                , style "border-color" "white"
                , style "border-style" "solid"
                , style "border-radius" (px diameter)
                , style "height" (px (diameter - borderWidth * 2))
                , style "width" (px (diameter - borderWidth * 2))
                , style "position" "absolute"
                , style "top" (px 0)
                , style "left" (px (toFloat i * length))
                , onClick msg
                ]
                []

        len =
            div
                [ style "background-color" color
                , style "border-width" (px borderWidth)
                , style "border-color" "white"
                , style "border-style" "solid"
                , style "border-radius" (px (diameter / 2))
                , style "height" (px ((diameter - borderWidth * 2) / 3))
                , style "width" (px (length + diameter / 3))
                , style "position" "absolute"
                , style "top" (px ((diameter - borderWidth * 2) / 3))
                , style "left" (px (toFloat i * length + (diameter - borderWidth * 2) / 3))
                , onClick msg
                ]
                []
    in
    if final then
        [ circle ]

    else
        [ circle, len ]


main : Program () Model Msg
main =
    element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
