module RtServerClient exposing (reportResult)

import Http exposing (Error, expectWhatever, header, jsonBody, request)
import Iso8601 exposing (fromTime)
import Json.Encode exposing (Value, float, int, object, string)
import Time exposing (Posix)


type alias RtResult =
    { averageSeconds : Float
    , successCount : Int
    , testCount : Int
    , dateTime : Posix
    }


rtResultToValue : RtResult -> Value
rtResultToValue { averageSeconds, successCount, testCount, dateTime } =
    object
        [ ( "averageSeconds", float averageSeconds )
        , ( "successCount", int successCount )
        , ( "testCount", int testCount )

        -- TODO: The Elm guys are probably right that this should just the Posix Time
        , ( "dateTime", string <| String.replace "Z" " UTC" <| String.replace "T" " " <| fromTime dateTime )
        ]


reportResult : String -> String -> RtResult -> Cmd (Result Error ())
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
        , expect = expectWhatever identity
        , timeout = Just 5000
        , tracker = Nothing
        }
