{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Lib
    (handler) where

import           AWS.Lambda.Events.ApiGatewayProxyRequest  (ApiGatewayProxyRequest (..),
                                                            authorizer)
import           AWS.Lambda.Events.ApiGatewayProxyResponse (ApiGatewayProxyResponse (..),
                                                            textPlain)
import           Control.Applicative                       (liftA2, (<|>))
import           Control.Lens                              (preview, set, view,
                                                            _2, _Just)
import           Control.Lens.At                           (at, ix)
import           Control.Monad.Trans                       (liftIO)
import           Data.Foldable                             (traverse_)
import           Data.HashMap.Strict                       (HashMap, fromList,
                                                            lookup)
import           Data.Maybe                                (fromJust)
import           Data.Text                                 (Text, pack)
import           Data.Text.Lazy                            (toStrict)
import           Data.Time.Clock                           (UTCTime, addUTCTime)
import           Network.AWS                               (MonadAWS, send)
import           Network.AWS.DynamoDB.PutItem              (PutItem, piItem,
                                                            putItem)
import           Network.AWS.DynamoDB.Query                (qExpressionAttributeNames,
                                                            qExpressionAttributeValues,
                                                            qKeyConditionExpression,
                                                            qLimit, qrsCount,
                                                            qrsItems,
                                                            qrsLastEvaluatedKey,
                                                            query)
import           Network.AWS.DynamoDB.Types                (AttributeValue,
                                                            attributeValue, avN,
                                                            avS)
import           Network.AWS.SNS.Publish                   (pMessage, pTopicARN,
                                                            publish)

import           Prelude                                   hiding (lookup)

import           Control.Monad                             (mzero)
import           Data.Aeson                                (FromJSON,
                                                            ToJSON (..),
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
import           System.IO                                 (hPutStr, stderr)

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

httpHandler :: MonadAWS m => Text -> ApiGatewayProxyRequest UserId -> m ApiGatewayProxyResponse
httpHandler tableName ApiGatewayProxyRequest { requestContext, body, httpMethod = "POST", pathParameters = lookup "testType" -> Just "rt"  } =
  case (decode body, authorizer requestContext) of
    (Just rtResult, Just (UserId userId)) -> do
      _ <- send $ putRtResult tableName userId rtResult
      return (ApiGatewayProxyResponse ok200 mempty (textPlain "Done"))
    (Just _, Nothing) ->
      return (ApiGatewayProxyResponse unauthorized401 mempty (textPlain "Unauthorized"))
    (Nothing, _) ->
      return (ApiGatewayProxyResponse badRequest400 mempty (textPlain "Bad Request"))
httpHandler tableName ApiGatewayProxyRequest { requestContext, body, httpMethod = "POST", pathParameters = lookup "testType" -> Just "poms"   } =
  case (decode body, authorizer requestContext) of
    (Just pomsResult, Just (UserId userId)) -> do
      _ <- send $ putPomsResult tableName userId pomsResult
      return (ApiGatewayProxyResponse ok200 mempty (textPlain "Done"))
    (Just _, Nothing) ->
      return (ApiGatewayProxyResponse unauthorized401 mempty (textPlain "Unauthorized"))
    (Nothing, _) ->
      return (ApiGatewayProxyResponse badRequest400 mempty (textPlain "Bad Request"))
httpHandler _ ApiGatewayProxyRequest { httpMethod = "GET" } =
  return (ApiGatewayProxyResponse notImplemented501 mempty (textPlain "Not Yet Implemented"))
httpHandler _ _ =
  return (ApiGatewayProxyResponse methodNotAllowed405 mempty (textPlain "Method Not Allowed"))

data CloudWatchEvent = CloudWatchEvent UTCTime

toReadableTimeStr :: String -> String
toReadableTimeStr str =
  let f 'T' = ' '
      f 'Z' = ' '
      f c   = c
  in fmap f str <> "UTC"

instance FromJSON CloudWatchEvent where
  parseJSON (Object v) =
    (CloudWatchEvent . read . toReadableTimeStr) <$> v .: "time"
  parseJSON _ = mzero

querySubscriptions ::
  MonadAWS m => Text -> m (Maybe (HashMap Text AttributeValue), [(Text, Text)])
querySubscriptions tableName = do
  res <- send
    $ set qKeyConditionExpression
        (Just "#primaryKey = :subscriptions")
    $ set qExpressionAttributeNames
        -- TODO: The table hash key should probably be named something more generic
        (fromList [("#primaryKey", "userId")])
    $ set qExpressionAttributeValues
        (fromList [(":subscriptions", set avS (Just "subscriptions") attributeValue)])
    $ query tableName
  -- TODO: handle decode failure
  let extractDetails hashMap =
        -- TODO: More include a list of all subscriptions
        fromJust $ liftA2 (,)
          (preview (at "topicArn" . _Just . avS . _Just) hashMap)
          -- Note that the LI for for subscriptions holds the userId
          (preview (at "LI" . _Just . avS . _Just) hashMap)
  return (preview qrsLastEvaluatedKey res, extractDetails <$> view qrsItems res)

userHasOneTestAfter :: MonadAWS m => Text -> UTCTime -> Text -> Text -> m Bool
userHasOneTestAfter tableName time userId testType = do
  res <- send
    $ set qLimit (Just 1)
    $ set qKeyConditionExpression (Just "#userId = :userId AND #dateTime > :dateTime")
    $ set qExpressionAttributeNames (fromList
        [ ("#dateTime", "LI")
        , ("#userId", "userId")
        ]
      )
    $ set qExpressionAttributeValues (fromList
        -- TODO: Query for other test types
        -- TODO: More general "key to text" strategy
        [ (":dateTime", set avS (Just $ (<>) (testType <> "#") $ pack $ show time) attributeValue)
        , (":userId", set avS (Just userId) attributeValue )
        ]
      )
    $ query tableName
  let count = fromJust $ view qrsCount res
  return $ count > 0

messageUserIfNeeded :: MonadAWS m => Text -> UTCTime -> Text -> Text -> Text -> m ()
messageUserIfNeeded tableName time userId topicArn testType = do
  isUpToDate <- userHasOneTestAfter tableName time userId testType
  if isUpToDate then
    liftIO $ hPutStr stderr "Records found, no SNS to publish."
  else do
    _ <- send $ set pTopicARN (Just topicArn) (publish "Get to it, bro!")
    return ()

cronHandler :: MonadAWS m => Text -> CloudWatchEvent -> m ()
cronHandler tableName (CloudWatchEvent time) =
  let
    twentyHoursPrior = addUTCTime (-60 * 60 * 20) time
    sendNotifications = \(x, y) -> messageUserIfNeeded tableName twentyHoursPrior x y "rt"
  in do
    --TODO: Continue to follow pagination
    (_, subs) <- querySubscriptions tableName
    --TODO: This could totally be concurrent, but struggling with the monad transforms
    traverse_ sendNotifications subs


data HandlerEvent
  = HttpEvent (ApiGatewayProxyRequest UserId)
  | CronEvent CloudWatchEvent

instance FromJSON HandlerEvent where
  parseJSON v =
    (HttpEvent <$> parseJSON v) <|> (CronEvent <$> parseJSON v)

data HandlerResponse
  = HttpResponse ApiGatewayProxyResponse
  | CronResponse ()

instance ToJSON HandlerResponse where
  toJSON (HttpResponse x) = toJSON x
  toJSON (CronResponse x) = toJSON x

--TODO: This whole strategy might be more trouble than it's worth
--and they should just be two entirely distinct lambdas.
--They should _at least_ be separated into separate modules.
handler :: MonadAWS m => Text -> HandlerEvent -> m HandlerResponse
handler tableName event =
  case event of
    HttpEvent e -> HttpResponse <$> httpHandler tableName e
    CronEvent e -> CronResponse <$> cronHandler tableName e
