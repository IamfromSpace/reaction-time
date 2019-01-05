module Main exposing (main)

import Browser exposing (element)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode exposing (field, string)
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub, batch)
import Random exposing (generate, int)


initialModel : Model
initialModel =
    ( [], Done 0 0 0 )


type alias Model =
    ( List (Maybe Float), State )


type State
    = Countdown Float
    | AwaitFinger
    | Ready Float Int
    | Done Float Int Int


type Msg
    = Tap Int
    | RandomFinger Int
    | Start
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Start, ( h, Done _ _ _ ) ) ->
            ( ( h, Countdown 1000 ), Cmd.none )

        ( Tap answer, ( h, Ready t correct ) ) ->
            ( ( (if answer == correct then
                    Just t

                 else
                    Nothing
                )
                    :: h
              , Done t correct answer
              )
            , Cmd.none
            )

        ( RandomFinger i, ( h, AwaitFinger ) ) ->
            ( ( h, Ready 0 i ), Cmd.none )

        ( Tick dt, ( h, Countdown t ) ) ->
            if t - dt <= 0 then
                ( ( h, AwaitFinger ), generate RandomFinger (int 0 3) )

            else
                ( ( h, Countdown (t - dt) ), Cmd.none )

        ( Tick dt, ( h, Ready t f ) ) ->
            ( ( h, Ready (t + dt) f ), Cmd.none )

        ( _, s ) ->
            ( s, Cmd.none )


spacer : Html a
spacer =
    div
        [ style "height" "100px"
        , style "width" "50px"
        ]
        []


box : Bool -> Html a
box isActive =
    div
        [ style "height" "100px"
        , style "width" "100px"
        , style "font-size" "100px"
        , style "text-align" "center"
        , style "background-color"
            (if isActive then
                "green"

             else
                "white"
            )
        , style "border" "1px solid black"
        ]
        [ text <|
            if isActive then
                "X"

            else
                ""
        ]


average : List (Maybe Float) -> Float
average xs =
    let
        filtered =
            List.filterMap identity xs
    in
    filtered
        |> List.foldr (+) 0
        |> (\x -> x / toFloat (List.length filtered))


counts : List (Maybe Float) -> ( Int, Int )
counts =
    List.partition ((==) Nothing) >> (\( a, b ) -> ( List.length a, List.length b ))


view : Model -> Html Msg
view ( history, state ) =
    case state of
        Done t i o ->
            div []
                [ div []
                    [ text <|
                        if i == o then
                            "correct"

                        else
                            "WRONG"
                    ]
                , div [] [ text <| (String.fromFloat (average history) ++ "ms") ]
                , div [] [ text <| (\( e, c ) -> String.fromInt (c - e) ++ "/" ++ String.fromInt c) <| counts history ]
                ]

        Ready _ i ->
            div [ style "flex-direction" "row", style "display" "flex" ] [ box (i == 0), box (i == 1), spacer, box (i == 2), box (i == 3) ]

        _ ->
            div [ style "flex-direction" "row", style "display" "flex" ] [ box False, box False, spacer, box False, box False ]


sub : Model -> Sub Msg
sub ( _, state ) =
    case state of
        Countdown _ ->
            onAnimationFrameDelta Tick

        Ready _ _ ->
            batch
                [ onAnimationFrameDelta Tick
                , onKeyDown
                    (Json.Decode.map
                        (\c ->
                            Tap <|
                                case c of
                                    "e" ->
                                        0

                                    "u" ->
                                        1

                                    "h" ->
                                        2

                                    "t" ->
                                        3

                                    _ ->
                                        -1
                        )
                        (field "key" string)
                    )
                ]

        Done _ _ _ ->
            onKeyDown
                (Json.Decode.map
                    (\x ->
                        if x == " " then
                            Start

                        else
                            RandomFinger -1
                    )
                    (field "key" string)
                )

        _ ->
            Sub.none


main : Program () Model Msg
main =
    element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = sub
        }
