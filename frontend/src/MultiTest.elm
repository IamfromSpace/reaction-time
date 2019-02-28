module MultiTest exposing (Model, Msg, initModel, sub, update, view)

import Html exposing (Html)
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)
import SingleTest


initModel : Model
initModel =
    { history = [], testState = SingleTest.initModel }


type alias Model =
    { history : List (Maybe Float)
    , testState : SingleTest.Model
    }


type Msg
    = SingleTestMsg SingleTest.Msg


type alias UpdateResult =
    { next : Model
    , notifications : Maybe (List (Maybe Float))
    , cmds : Cmd Msg
    }


update : Int -> Msg -> Model -> UpdateResult
update targetCount msg ({ history, testState } as model) =
    if List.length history == targetCount then
        { next = model, notifications = Nothing, cmds = Cmd.none }

    else
        case msg of
            SingleTestMsg m ->
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
                        in
                        { next = { model | testState = SingleTest.initModel, history = history_ }
                        , notifications =
                            if List.length history_ == targetCount then
                                Just history_

                            else
                                Nothing
                        , cmds = Cmd.map SingleTestMsg ur.cmds
                        }


view : Model -> Html Msg
view { testState } =
    Html.map SingleTestMsg <| SingleTest.view testState


sub : Model -> Sub Msg
sub { testState } =
    Sub.map SingleTestMsg <| SingleTest.sub testState
