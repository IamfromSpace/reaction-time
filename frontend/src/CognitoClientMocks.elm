module CognitoClientMocks exposing (alwaysRequireChangePasswordWithSession, alwaysSucceedChangePasswordWithTokens, alwaysSucceedRefreshWithTokens)

import CognitoClient exposing (AnswerNewPasswordChallenge, Login, LoginResult(..), RefreshSession, TokenSet)
import Task


alwaysRequireChangePasswordWithSession : String -> Login
alwaysRequireChangePasswordWithSession session _ _ =
    Task.perform identity <| Task.succeed (Ok (NewPasswordRequired session))


alwaysSucceedChangePasswordWithTokens : TokenSet -> AnswerNewPasswordChallenge
alwaysSucceedChangePasswordWithTokens tokens _ _ _ =
    Task.perform identity <| Task.succeed (Ok tokens)


alwaysSucceedRefreshWithTokens : TokenSet -> RefreshSession
alwaysSucceedRefreshWithTokens tokens _ =
    Task.perform identity <| Task.succeed (Ok tokens)
