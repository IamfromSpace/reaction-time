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
import           Control.Applicative                       ((<|>))
import           Control.Lens                              (preview, set, view,
                                                            _2, _Just)
import           Control.Lens.At                           (at, ix)
import           Control.Monad                             ((>=>))
import           Control.Monad.Trans                       (liftIO)
import           Data.Foldable                             (traverse_)
import           Data.HashMap.Strict                       (HashMap, fromList,
                                                            lookup)
import           Data.Maybe                                (fromJust, fromMaybe)
import           Data.Set                                  (Set)
import qualified Data.Set                                  as Set
import           Data.Text                                 (Text, pack, intercalate)
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
                                                            avS, avSS)
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

class FromDynamoDBItem a where
  fromItem :: HashMap Text AttributeValue -> Maybe a

data TestType
  = ReactionTime
  | POMS
  deriving (Eq, Ord)

instance Show TestType where
  show ReactionTime = "rt"
  show POMS = "poms"

parseTestType :: Text -> Maybe TestType
parseTestType "rt" = Just ReactionTime
parseTestType "poms" = Just POMS
parseTestType _ = Nothing

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
      , ("testType", set avS (Just (pack (show ReactionTime))) attributeValue)
      , ("LI", set avS (Just (pack (show ReactionTime ++ "#" ++ show dateTime))) attributeValue)
      , ("LSI", set avS (Just (pack (show dateTime ++ "#" ++ show ReactionTime))) attributeValue)
      , ("userId", set avS (Just userId) attributeValue)
      ]

putPomsResult :: Text -> Text -> PomsResult -> PutItem
putPomsResult tableName userId pomsResult =
  set piItem (toPomsResultItem userId pomsResult) (putItem tableName)

testTypeFromPathParams :: HashMap Text Text -> Maybe TestType
testTypeFromPathParams =
  lookup "testType" >=> parseTestType

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
      , ("LI", set avS (Just (pack (show POMS ++ "#" ++ show dateTime))) attributeValue)
      , ("LSI", set avS (Just (pack (show dateTime ++ "#" ++ show POMS))) attributeValue)
      , ("dateTimeUTC", set avS (Just $ pack $ show dateTime) attributeValue)
      , ("testType", set avS (Just (pack (show POMS))) attributeValue)
      , ("userId", set avS (Just userId) attributeValue)
      ]

httpHandler :: MonadAWS m => Text -> ApiGatewayProxyRequest UserId -> m ApiGatewayProxyResponse
httpHandler tableName ApiGatewayProxyRequest { requestContext, body, httpMethod = "POST", pathParameters = testTypeFromPathParams -> Just ReactionTime } =
  case (decode body, authorizer requestContext) of
    (Just rtResult, Just (UserId userId)) -> do
      _ <- send $ putRtResult tableName userId rtResult
      return (ApiGatewayProxyResponse ok200 mempty (textPlain "Done"))
    (Just _, Nothing) ->
      return (ApiGatewayProxyResponse unauthorized401 mempty (textPlain "Unauthorized"))
    (Nothing, _) ->
      return (ApiGatewayProxyResponse badRequest400 mempty (textPlain "Bad Request"))
httpHandler tableName ApiGatewayProxyRequest { requestContext, body, httpMethod = "POST", pathParameters = testTypeFromPathParams -> Just POMS } =
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

data TestsReminderSub = TestsReminderSub
  { userId :: Text
  , topicArn :: Text
  , testTypes :: Set TestType
  }

instance FromDynamoDBItem TestsReminderSub where
  fromItem hashMap = TestsReminderSub <$>
    -- Note that the LI for for subscriptions holds the userId
    (view avS =<< lookup "LI" hashMap) <*>
    (view avS =<< lookup "topicArn" hashMap) <*>
    (Set.fromList <$> (traverse parseTestType . view avSS =<< lookup "testTypes" hashMap))

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
  MonadAWS m => Text -> m (Maybe (HashMap Text AttributeValue), [TestsReminderSub])
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
  return (preview qrsLastEvaluatedKey res, fromJust . fromItem <$> view qrsItems res)

userHasOneTestBetween :: MonadAWS m => Text -> UTCTime -> UTCTime -> Text -> TestType -> m Bool
userHasOneTestBetween tableName a b userId testType = do
  res <- send
    $ set qLimit (Just 1)
    $ set qKeyConditionExpression (Just "#userId = :userId AND #testDateTime BETWEEN :testA AND :testB")
    $ set qExpressionAttributeNames (fromList
        [ ("#testDateTime", "LI")
        , ("#userId", "userId")
        ]
      )
    $ set qExpressionAttributeValues (fromList
        -- TODO: More general "key to text" strategy
        [ (":testA", set avS (Just $ pack $ (<>) (show testType <> "#") $ show a) attributeValue)
        , (":testB", set avS (Just $ pack $ (<>) (show testType <> "#") $ show b) attributeValue)
        , (":userId", set avS (Just userId) attributeValue )
        ]
      )
    $ query tableName
  let count = fromJust $ view qrsCount res
  return $ count > 0

filterByList :: [Bool] -> [a] -> [a]
filterByList (b:bt) (a:at) = if b then a : filterByList bt at else filterByList bt at
filterByList [] _ = []
filterByList _ [] = []

messageUserIfNeeded :: MonadAWS m => Text -> UTCTime -> TestsReminderSub -> m ()
messageUserIfNeeded tableName now TestsReminderSub { userId, topicArn, testTypes = tts } = do
  let testTypes = Set.toList tts
  let twentyHoursPrior = addUTCTime (-60 * 60 * 20) now
  let checkIsTestTypeUpToDate = userHasOneTestBetween tableName twentyHoursPrior now userId
  -- TODO: We could check each test type concurrently, traverse will be serial
  isUpToDateListOrdered <- traverse checkIsTestTypeUpToDate testTypes
  let testTypesThatNeedReminders = filterByList (not <$> isUpToDateListOrdered) testTypes
  case testTypesThatNeedReminders of
    [] -> liftIO $ hPutStr stderr "Records found for all subcribed types, no SNS to publish."
    _ -> do
      let msg =
            "Get to it, bro!  Need to do: " <>
            intercalate " and " (pack . show <$> testTypesThatNeedReminders) <>
            "."
      _ <- send $ set pTopicARN (Just topicArn) $ publish msg
      return ()

cronHandler :: MonadAWS m => Text -> CloudWatchEvent -> m ()
cronHandler tableName (CloudWatchEvent time) = do
  --TODO: Continue to follow pagination
  (_, subs) <- querySubscriptions tableName
  --TODO: This could totally be concurrent, but struggling with the monad transforms
  traverse_ (messageUserIfNeeded tableName time) subs


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
