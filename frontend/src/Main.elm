module Main exposing (main)

import Browser exposing (element)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick, preventDefaultOn)
import Json.Decode exposing (field, string, succeed)
import Login
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub, batch)
import Random exposing (generate, int)
import RtServerClient exposing (reportResult)
import Task exposing (perform)
import Time exposing (Posix, now)



-- TODO: Fully break AppMsg and Msg into submodules


initialModel : Model
initialModel =
    { history = [], state = Done 0 0 0, remainingCount = 80, loginState = NotLoggedIn }


type alias Model =
    { history : List (Maybe Float)
    , state : State
    , remainingCount : Int
    , loginState : LoginState
    }


type LoginState
    = NotLoggedIn
    | LoggingIn Login.Model
    | LoggedIn String


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


type AppMsg
    = TestMsg Msg
    | LoginMsg Login.Msg
    | StartLogin
    | Submit
    | SubmitReady Posix
    | ReceiveSubmitResult


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


update : AppMsg -> Model -> ( Model, Cmd AppMsg )
update msg { history, state, remainingCount, loginState } =
    case ( msg, loginState ) of
        ( StartLogin, NotLoggedIn ) ->
            ( { history = history, state = state, remainingCount = remainingCount, loginState = LoggingIn Login.initModel }, Cmd.none )

        ( LoginMsg m, LoggingIn x ) ->
            let
                ( ( next, cmds ), maybeNotifications ) =
                    Login.update "1hc2128v5ffkeokim4uvbpjmj7" m x
            in
            case maybeNotifications of
                Nothing ->
                    ( { history = history, state = state, remainingCount = remainingCount, loginState = LoggingIn next }, Cmd.map LoginMsg cmds )

                Just token ->
                    ( { history = history, state = state, remainingCount = remainingCount, loginState = LoggedIn token }, Cmd.none )

        ( TestMsg testMsg, _ ) ->
            let
                { next, notifications, cmds } =
                    updateState testMsg state
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
                      , loginState = loginState
                      }
                    , Cmd.map TestMsg cmds
                    )

                Nothing ->
                    ( { history = history, state = next, remainingCount = remainingCount, loginState = loginState }
                    , Cmd.map TestMsg cmds
                    )

        ( Submit, _ ) ->
            ( { history = history, state = state, remainingCount = remainingCount, loginState = loginState }
            , if remainingCount == 0 then
                perform SubmitReady now

              else
                Cmd.none
            )

        ( SubmitReady posix, LoggedIn token ) ->
            let
                ( errorCount, successCount ) =
                    counts history
            in
            ( { history = history, state = state, remainingCount = remainingCount, loginState = loginState }
              -- TODO: Don't just drop the error on the floor
            , Cmd.map (always ReceiveSubmitResult) <|
                reportResult
                    -- TODO: Endpoint setup
                    "https://xxxxxxxxxx.execute-api.us-east-1.amazonaws.com/default/rt-tester"
                    token
                    { averageSeconds = average history
                    , successCount = successCount
                    , testCount = errorCount + successCount
                    , dateTime = posix
                    }
            )

        _ ->
            ( { history = history, state = state, remainingCount = remainingCount, loginState = loginState }
            , Cmd.none
            )


spacer : Html Msg
spacer =
    div
        [ style "height" "17.5vw"
        , style "width" "10vw"
        , preventDefaultOn "touchstart" (succeed ( Start, True ))
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


addSpacer : List (Html Msg) -> List (Html Msg)
addSpacer x =
    case x of
        a :: b :: t ->
            a :: b :: spacer :: t

        y ->
            y


view : Model -> Html AppMsg
view { history, state, remainingCount, loginState } =
    case loginState of
        LoggingIn m ->
            Html.map LoginMsg (Login.view m)

        _ ->
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
                    , Html.map TestMsg <|
                        div [ style "flex-direction" "row", style "display" "flex" ]
                            (addSpacer (List.indexedMap (Tap >> box) colors))
                    , div
                        [ style "display" "flex"
                        , style "justify-content" "space-between"
                        , style "flex-direction" "row"
                        , style "color"
                            (if remainingCount == 0 then
                                "black"

                             else
                                "#00000000"
                            )
                        ]
                        [ div [] [ text <| (pastDecimal 1 (String.fromFloat (average history)) ++ "ms") ]
                        , div [] [ text <| (\( e, c ) -> String.fromInt c ++ "/" ++ String.fromInt (c + e)) <| counts history ]
                        ]
                    , case loginState of
                        LoggedIn _ ->
                            button [ onClick Submit, disabled (remainingCount /= 0) ] [ text "Submit" ]

                        _ ->
                            button [ onClick StartLogin ] [ text "Login" ]
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


main : Program () Model AppMsg
main =
    element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = Sub.map TestMsg << sub
        }
