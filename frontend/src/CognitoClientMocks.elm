module CognitoClientMocks exposing (alwaysRequireChangePasswordWithSession, alwaysSucceedChangePasswordWithToken)

import CognitoClient exposing (AnswerNewPasswordChallenge, Login, LoginResult(..))
import Task


alwaysRequireChangePasswordWithSession : String -> Login
alwaysRequireChangePasswordWithSession session _ _ =
    Task.perform identity <| Task.succeed (Ok (NewPasswordRequired session))


alwaysSucceedChangePasswordWithToken : String -> AnswerNewPasswordChallenge
alwaysSucceedChangePasswordWithToken token _ _ _ =
    Task.perform identity <| Task.succeed (Ok token)
