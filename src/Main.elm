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
    { history = [], state = Done 0 0 0, remainingCount = 80 }


type alias Model =
    { history : List (Maybe Float)
    , state : State
    , remainingCount : Int
    }


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


type alias UpdateResult =
    { next : State
    , notifications : Maybe (Maybe Float)
    , cmds : Cmd Msg
    }


updateState : Msg -> State -> UpdateResult
updateState msg state =
    case ( msg, state ) of
        ( Start, Done _ _ _ ) ->
            { next = Countdown 1000, notifications = Nothing, cmds = Cmd.none }

        ( Tap answer, Ready t correct ) ->
            { next = Done t correct answer
            , notifications =
                Just <|
                    if answer == correct then
                        Just t

                    else
                        Nothing
            , cmds = Cmd.none
            }

        ( RandomFinger i, AwaitFinger ) ->
            { next = Ready 0 i, notifications = Nothing, cmds = Cmd.none }

        ( Tick dt, Countdown t ) ->
            if t - dt <= 0 then
                { next = AwaitFinger, notifications = Nothing, cmds = generate RandomFinger (int 0 3) }

            else
                { next = Countdown (t - dt), notifications = Nothing, cmds = Cmd.none }

        ( Tick dt, Ready t f ) ->
            { next = Ready (t + dt) f, notifications = Nothing, cmds = Cmd.none }

        ( _, s ) ->
            { next = s, notifications = Nothing, cmds = Cmd.none }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { history, state, remainingCount } =
    let
        { next, notifications, cmds } =
            updateState msg state
    in
    case notifications of
        Just m ->
            ( { history = m :: history
              , state =
                    if remainingCount <= 1 then
                        next

                    else
                        Countdown 1000
              , remainingCount = remainingCount - 1
              }
            , cmds
            )

        Nothing ->
            ( { history = history, state = next, remainingCount = remainingCount }, cmds )


spacer : Html a
spacer =
    div
        [ style "height" "100px"
        , style "width" "50px"
        ]
        []


box : String -> Html a
box color =
    div
        [ style "height" "100px"
        , style "width" "100px"
        , style "font-size" "100px"
        , style "text-align" "center"
        , style "background-color" color
        , style "border" "1px solid black"
        ]
        []


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


addSpacer : List (Html a) -> List (Html a)
addSpacer x =
    case x of
        a :: b :: t ->
            a :: b :: spacer :: t

        y ->
            y


view : Model -> Html Msg
view { history, state } =
    let
        colors =
            case state of
                Done _ i o ->
                    List.map
                        (\x ->
                            if i == o then
                                "green"

                            else
                                "red"
                        )
                        [ 0, 1, 2, 3 ]

                Ready _ i ->
                    List.map
                        (\x ->
                            if x == i then
                                "green"

                            else
                                "white"
                        )
                        [ 0, 1, 2, 3 ]

                _ ->
                    [ "white", "white", "white", "white" ]
    in
    div [ style "display" "flex", style "justify-content" "center", style "align-items" "center", style "height" "90vh" ]
        [ div
            []
            [ div [ style "margin" "14px", style "font-size" "28px", style "display" "flex", style "justify-content" "center" ] [ text "Reaction Time Tester" ]
            , div [ style "flex-direction" "row", style "display" "flex" ] (addSpacer (List.map box colors))
            , div [ style "display" "flex", style "justify-content" "space-between", style "flex-direction" "row" ]
                [ div [] [ text <| (pastDecimal 1 (String.fromFloat (average history)) ++ "ms") ]
                , div [] [ text <| (\( e, c ) -> String.fromInt c ++ "/" ++ String.fromInt (c + e)) <| counts history ]
                ]
            ]
        ]



-- Drop anything n characters after a period
-- Note that for something like `pastDecimal 1 "3.0000000002"`
-- this will not return "3"


pastDecimal : Int -> String -> String
pastDecimal n s =
    let
        go built str =
            case str of
                '.' :: t ->
                    List.reverse built ++ '.' :: List.take n t

                h :: t ->
                    go (h :: built) t

                [] ->
                    List.reverse built
    in
    String.fromList <| go [] <| String.toList s


sub : Model -> Sub Msg
sub { state } =
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
