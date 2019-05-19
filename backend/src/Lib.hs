{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
module Lib
    (handler) where

import           Prelude                                   hiding (lookup)
import           AWS.Lambda.Events.ApiGatewayProxyRequest  (ApiGatewayProxyRequest (..), authorizer)
import           AWS.Lambda.Events.ApiGatewayProxyResponse (ApiGatewayProxyResponse (..),
                                                            textPlain)
import           Control.Lens                              (set)
import           Data.HashMap.Strict                       (HashMap, fromList, lookup)
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
import           Network.HTTP.Types.Status                 (Status (..),
                                                            badRequest400,
                                                            methodNotAllowed405,
                                                            notImplemented501,
                                                            ok200,
                                                            unauthorized401)

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

data PomsResult = PomsResult
  { tmd        :: Int
  , tension    :: Int
  , depression :: Int
  , anger      :: Int
  , fatigue    :: Int
  , confusion  :: Int
  , vigor      :: Int
  , dateTime   :: UTCTime
  }

instance FromJSON PomsResult where
  parseJSON (Object v) =
    PomsResult <$>
    v .: "tmd" <*>
    v .: "tension" <*>
    v .: "depression" <*>
    v .: "anger" <*>
    v .: "fatigue" <*>
    v .: "confusion" <*>
    v .: "vigor" <*>
    (read <$> v .: "dateTime")
  parseJSON _ = mzero

data UserId = UserId Text

instance FromJSON UserId where
  parseJSON (Object v) =
    UserId <$> (v .: "claims" >>= (\(Object v2) -> v2 .: "sub"))

putRtResult :: Text -> Text -> RtResult -> PutItem
putRtResult tableName userId rtResult =
  set piItem (toRtResultItem userId rtResult) (putItem tableName)

toRtResultItem :: Text -> RtResult -> HashMap Text AttributeValue
toRtResultItem userId RtResult { averageSeconds, successCount, testCount, dateTime } =
    fromList
      [ ("averageReactionTimeSeconds", set avN (Just $ pack $ show averageSeconds) attributeValue)
      , ("successCount", set avN (Just $ pack $ show successCount) attributeValue)
      , ("testCount", set avN (Just $ pack $ show testCount) attributeValue)
      , ("dateTimeUTC", set avS (Just $ pack $ show dateTime) attributeValue)
      , ("testType", set avS (Just "rt") attributeValue)
      , ("LI", set avS (Just (pack ("rt#" ++ show dateTime))) attributeValue)
      , ("LSI", set avS (Just (pack (show dateTime ++ "#rt"))) attributeValue)
      , ("userId", set avS (Just userId) attributeValue)
      ]

putPomsResult :: Text -> Text -> PomsResult -> PutItem
putPomsResult tableName userId pomsResult =
  set piItem (toPomsResultItem userId pomsResult) (putItem tableName)

toPomsResultItem :: Text -> PomsResult -> HashMap Text AttributeValue
toPomsResultItem userId pr@PomsResult { dateTime, tmd, tension, depression, anger, fatigue, confusion, vigor } =
    fromList
      [ ("tmd", set avN (Just $ pack $ show tmd) attributeValue)
      , ("tension", set avN (Just $ pack $ show tension) attributeValue)
      , ("depression", set avN (Just $ pack $ show depression) attributeValue)
      , ("anger", set avN (Just $ pack $ show anger) attributeValue)
      , ("fatigue", set avN (Just $ pack $ show fatigue) attributeValue)
      , ("confusion", set avN (Just $ pack $ show confusion) attributeValue)
      , ("vigor", set avN (Just $ pack $ show vigor) attributeValue)
      , ("LI", set avS (Just (pack ("poms#" ++ show dateTime))) attributeValue)
      , ("LSI", set avS (Just (pack (show dateTime ++ "#poms"))) attributeValue)
      , ("dateTimeUTC", set avS (Just $ pack $ show dateTime) attributeValue)
      , ("userId", set avS (Just userId) attributeValue)
      ]

handler :: MonadAWS m => Text -> ApiGatewayProxyRequest UserId -> m ApiGatewayProxyResponse
handler tableName ApiGatewayProxyRequest { requestContext, body, httpMethod = "POST", pathParameters = lookup "testType" -> Just "rt" } =
  case (decode body, authorizer requestContext) of
    (Just rtResult, Just (UserId userId)) -> do
      _ <- send $ putRtResult tableName userId rtResult
      return (ApiGatewayProxyResponse ok200 mempty (textPlain "Done"))
    (Just _, Nothing) ->
      return (ApiGatewayProxyResponse unauthorized401 mempty (textPlain "Unauthorized"))
    (Nothing, _) ->
      return (ApiGatewayProxyResponse badRequest400 mempty (textPlain "Bad Request"))
handler tableName ApiGatewayProxyRequest { requestContext, body, httpMethod = "POST", pathParameters = lookup "testType" -> Just "poms"  } =
  case (decode body, authorizer requestContext) of
    (Just pomsResult, Just (UserId userId)) -> do
      _ <- send $ putPomsResult tableName userId pomsResult
      return (ApiGatewayProxyResponse ok200 mempty (textPlain "Done"))
    (Just _, Nothing) ->
      return (ApiGatewayProxyResponse unauthorized401 mempty (textPlain "Unauthorized"))
    (Nothing, _) ->
      return (ApiGatewayProxyResponse badRequest400 mempty (textPlain "Bad Request"))
handler _ ApiGatewayProxyRequest { httpMethod = "GET" } =
  return (ApiGatewayProxyResponse notImplemented501 mempty (textPlain "Not Yet Implemented"))
handler _ _ =
  return (ApiGatewayProxyResponse methodNotAllowed405 mempty (textPlain "Method Not Allowed"))
