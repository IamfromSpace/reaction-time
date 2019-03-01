module MultiTest exposing (Model, Msg, initModel, sub, update, view)

import Html exposing (Html)
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)
import SingleTest
import Task exposing (perform)
import Time exposing (Posix, now)


initModel : Model
initModel =
    { history = [], testState = SingleTest.initModel }


type alias Model =
    { history : List (Maybe Float)
    , testState : SingleTest.Model
    }


type Msg
    = SingleTestMsg SingleTest.Msg
    | Now Posix


type alias UpdateResult =
    { next : Model
    , notifications : Maybe ( Posix, List (Maybe Float) )
    , cmds : Cmd Msg
    }


update : Int -> Msg -> Model -> UpdateResult
update targetCount msg ({ history, testState } as model) =
    case msg of
        Now posix ->
            { next = model, notifications = Just ( posix, history ), cmds = Cmd.none }

        SingleTestMsg m ->
            if List.length history == targetCount then
                { next = model, notifications = Nothing, cmds = Cmd.none }

            else
                let
                    ur =
                        SingleTest.update m testState
                in
                case ur.notifications of
                    Nothing ->
                        { next = { model | testState = ur.next }
                        , notifications = Nothing
                        , cmds = Cmd.map SingleTestMsg ur.cmds
                        }

                    Just testResult ->
                        let
                            history_ =
                                testResult :: history

                            urCmds =
                                Cmd.map SingleTestMsg ur.cmds
                        in
                        { next = { model | testState = SingleTest.initModel, history = history_ }
                        , notifications = Nothing
                        , cmds =
                            if List.length history_ == targetCount then
                                Cmd.batch [ perform Now now, urCmds ]

                            else
                                urCmds
                        }


view : Model -> Html Msg
view { testState } =
    Html.map SingleTestMsg <| SingleTest.view testState


sub : Model -> Sub Msg
sub { testState } =
    Sub.map SingleTestMsg <| SingleTest.sub testState
