module RtServerClient exposing (RtError(..), RtReporter, RtResult, reportResult, successReporter)

import Http exposing (Error(..), expectString, header, jsonBody, request)
import Iso8601 exposing (fromTime)
import Json.Encode exposing (Value, float, int, object, string)
import Task
import Time exposing (Posix)


type alias RtResult =
    { averageSeconds : Float
    , successCount : Int
    , testCount : Int
    , dateTime : Posix
    }


type RtError
    = MaybeRetry
    | RetryLater
    | Misconfiguration


errorToRtError : Error -> RtError
errorToRtError e =
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


rtResultToValue : RtResult -> Value
rtResultToValue { averageSeconds, successCount, testCount, dateTime } =
    object
        [ ( "averageSeconds", float averageSeconds )
        , ( "successCount", int successCount )
        , ( "testCount", int testCount )

        -- TODO: The Elm guys are probably right that this should just the Posix Time
        , ( "dateTime", string <| String.replace "Z" " UTC" <| String.replace "T" " " <| fromTime dateTime )
        ]


type alias RtReporter =
    RtResult -> Cmd (Maybe RtError)


reportResult : String -> String -> RtReporter
reportResult url token rtResult =
    request
        { method = "POST"
        , headers =
            [ header "Authorization" ("Bearer " ++ token) ]
        , url = url
        , body =
            rtResult
                |> rtResultToValue
                |> jsonBody
        , expect =
            -- expectWhatever seems to be bugged?
            expectString
                (\r ->
                    case r of
                        Ok _ ->
                            Nothing

                        Err e ->
                            Just <| errorToRtError e
                )
        , timeout = Just 5000
        , tracker = Nothing
        }


successReporter : RtReporter
successReporter _ =
    Task.perform identity <| Task.succeed Nothing
