module SingleTest exposing (Model, Msg, initModel, sub, update, view)

import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (preventDefaultOn)
import Json.Decode exposing (field, string, succeed)
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub, batch)
import Random exposing (generate, int)


initModel : Model
initModel =
    Countdown 1000


type Model
    = Countdown Float
    | AwaitFinger
    | Ready Float Int
    | Done Float Int Int


type Msg
    = Tap Int
    | RandomFinger Int
    | Tick Float


type alias UpdateResult =
    { next : Model
    , notifications : Maybe (Maybe Float)
    , cmds : Cmd Msg
    }


update : Msg -> Model -> UpdateResult
update msg model =
    case ( msg, model ) of
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


spacer : Html Msg
spacer =
    div
        [ style "height" "17.5vw"
        , style "width" "10vw"
        ]
        []


box : Msg -> String -> Html Msg
box event color =
    div
        [ style "height" "17.5vw"
        , style "width" "17.5vw"
        , style "text-align" "center"
        , style "background-color" color
        , style "border" "1px solid black"
        , preventDefaultOn "touchstart" (succeed ( event, True ))
        ]
        []


addSpacer : List (Html Msg) -> List (Html Msg)
addSpacer x =
    case x of
        a :: b :: t ->
            a :: b :: spacer :: t

        y ->
            y


view : Model -> Html Msg
view model =
    let
        colors =
            case model of
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
    div
        [ style "flex-direction" "row", style "display" "flex" ]
        (addSpacer (List.indexedMap (Tap >> box) colors))


sub : Model -> Sub Msg
sub model =
    case model of
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

        _ ->
            Sub.none
