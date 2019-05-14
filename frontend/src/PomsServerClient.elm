module PomsServerClient exposing (PomsError(..), PomsReporter, PomsResult, reportResult, successReporter)

import Http exposing (Error(..), expectString, header, jsonBody, request)
import Iso8601 exposing (fromTime)
import Json.Encode exposing (Value, float, int, object, string)
import Poms
import Task
import Time exposing (Posix)


type alias PomsResult =
    { tmd : Int
    , subTotal : Poms.SubTotal
    , dateTime : Posix
    }


type PomsError
    = MaybeRetry
    | RetryLater
    | Misconfiguration


errorToPomsError : Error -> PomsError
errorToPomsError e =
    case e of
        Timeout ->
            RetryLater

        NetworkError ->
            MaybeRetry

        BadStatus x ->
            if x >= 500 then
                RetryLater

            else
                Misconfiguration

        _ ->
            Misconfiguration


pomsResultToValue : PomsResult -> Value
pomsResultToValue { tmd, subTotal, dateTime } =
    object
        [ ( "tmd", int tmd )
        , ( "tension", int subTotal.tension )
        , ( "depression", int subTotal.depression )
        , ( "anger", int subTotal.anger )
        , ( "fatigue", int subTotal.fatigue )
        , ( "confusion", int subTotal.confusion )
        , ( "vigor", int subTotal.vigor )
        , ( "dateTime", string <| String.replace "Z" " UTC" <| String.replace "T" " " <| fromTime dateTime )
        ]


type alias PomsReporter =
    PomsResult -> Cmd (Maybe PomsError)


reportResult : String -> String -> PomsReporter
reportResult url token pomsResult =
    request
        { method = "POST"
        , headers =
            [ header "Authorization" ("Bearer " ++ token) ]
        , url = url
        , body =
            pomsResult
                |> pomsResultToValue
                |> jsonBody
        , expect =
            -- expectWhatever seems to be bugged?
            expectString
                (\r ->
                    case r of
                        Ok _ ->
                            Nothing

                        Err e ->
                            Just <| errorToPomsError e
                )
        , timeout = Just 5000
        , tracker = Nothing
        }


successReporter : PomsReporter
successReporter _ =
    Task.perform identity <| Task.succeed Nothing
