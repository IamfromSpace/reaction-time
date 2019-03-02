{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Lib
    (handler) where

import           AWS.Lambda.Events.ApiGatewayProxyRequest  (ApiGatewayProxyRequest (..),
                                                            cognitoIdentityId,
                                                            identity)
import           AWS.Lambda.Events.ApiGatewayProxyResponse (ApiGatewayProxyResponse (..))
import           Control.Lens                              (set)
import           Data.HashMap.Strict                       (HashMap, fromList)
import           Data.Text                                 (Text, pack)
import           Data.Text.Lazy                            (toStrict)
import           Data.Time.Clock                           (UTCTime)
import           Network.AWS                               (MonadAWS, send)
import           Network.AWS.DynamoDB.PutItem              (PutItem, piItem,
                                                            putItem)
import           Network.AWS.DynamoDB.Types                (AttributeValue,
                                                            attributeValue, avN,
                                                            avS)

import           Control.Monad                             (mzero)
import           Data.Aeson                                (FromJSON,
                                                            Value (Object),
                                                            decode, parseJSON,
                                                            (.:))
import           GHC.Generics                              (Generic)

data RtResult = RtResult
  { averageSeconds :: Float
  , successCount   :: Int
  , testCount      :: Int
  , dateTime       :: UTCTime
  }

instance FromJSON RtResult where
  parseJSON (Object v) =
    RtResult <$>
    v .: "averageSeconds" <*>
    v .: "successCount" <*>
    v .: "testCount" <*>
    (read <$> v .: "dateTime")
  parseJSON _ = mzero

data Tbd = Tbd
  { tbd1 :: Text
  , tbd2 :: Text
  } deriving Generic

instance FromJSON Tbd

putRtResult :: Text -> Text -> RtResult -> PutItem
putRtResult tableName cognitoIdentityId rtResult =
  set piItem (toRtResultItem cognitoIdentityId rtResult) (putItem tableName)

toRtResultItem :: Text -> RtResult -> HashMap Text AttributeValue
toRtResultItem cognitoIentityId RtResult { averageSeconds, successCount, testCount, dateTime } =
    fromList
      [ ("averageReactionTimeSeconds", set avN (Just $ pack $ show averageSeconds) attributeValue)
      , ("successCount", set avN (Just $ pack $ show successCount) attributeValue)
      , ("testCount", set avN (Just $ pack $ show testCount) attributeValue)
      , ("dateTimeUTC", set avS (Just $ pack $ show dateTime) attributeValue)
      , ("cognitoIdentityId", set avS (Just cognitoIentityId) attributeValue)
      ]

handler :: MonadAWS m => Text -> ApiGatewayProxyRequest -> m ApiGatewayProxyResponse
handler tableName ApiGatewayProxyRequest { requestContext, body, httpMethod = "POST"} =
  case (decode body, cognitoIdentityId (identity requestContext)) of
    (Just rtResult, Just cogId) -> do
      _ <- send $ putRtResult tableName (toStrict cogId) rtResult
      return (ApiGatewayProxyResponse 200 mempty "Done")
    (Just _, Nothing) ->
      return (ApiGatewayProxyResponse 401 mempty "Unauthorized")
    (Nothing, _) ->
      return (ApiGatewayProxyResponse 400 mempty "Bad Request")
handler _ ApiGatewayProxyRequest { body, httpMethod = "PUT" } =
  case decode body of
    Just Tbd { tbd1 } ->
      return (ApiGatewayProxyResponse 500 mempty tbd1)
    Nothing ->
      return (ApiGatewayProxyResponse 400 mempty "Bad Request")
handler _ ApiGatewayProxyRequest { httpMethod = "GET" } =
  return (ApiGatewayProxyResponse 501 mempty "Not Yet Implemented")
handler _ _ =
  return (ApiGatewayProxyResponse 405 mempty "Method Not Supported")
