{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Lib
    (handler) where

import           AWS.Lambda.Events.ApiGatewayProxyRequest  (ApiGatewayProxyRequest (..))
import           AWS.Lambda.Events.ApiGatewayProxyResponse (ApiGatewayProxyResponse (..))
import           Control.Lens                              (set)
import           Data.HashMap.Strict                       (HashMap, fromList)
import           Data.Text                                 (Text, pack)
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
                                                            parseJSON, (.:))

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

putRtResult :: Text -> RtResult -> PutItem
putRtResult tableName rtResult =
  set piItem (toRtResultItem rtResult) (putItem tableName)

toRtResultItem :: RtResult -> HashMap Text AttributeValue
toRtResultItem RtResult { averageSeconds, successCount, testCount, dateTime } =
    fromList
      [ ("averageReactionTimeSeconds", set avN (Just $ pack $ show averageSeconds) attributeValue)
      , ("successCount",  set avN (Just $ pack $ show successCount) attributeValue)
      , ("testCount",  set avN (Just $ pack $ show testCount) attributeValue)
      , ("dateTimeUTC",  set avS (Just $ pack $ show dateTime) attributeValue)
      ]

handler :: MonadAWS m => ApiGatewayProxyRequest RtResult -> m ApiGatewayProxyResponse
handler ApiGatewayProxyRequest { body } = do
  _ <- send (putRtResult "rt-tester" body)
  return (ApiGatewayProxyResponse 200 mempty "Done")
