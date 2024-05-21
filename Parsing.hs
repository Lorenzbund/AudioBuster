{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Parsing ( DeviceInfo(..)
               , DeviceType(..)
               , StreamInfo(..)
               , StreamType(..)
               , module Data.Aeson
               , module Data.Aeson.Types
               , module Data.Maybe
               , module Data.String
               )
where

import "aeson" Data.Aeson
import "aeson" Data.Aeson.Types
import GHC.Generics
import Data.Text qualified as T
import Data.ByteString qualified as B
import Data.String
import Data.Maybe

data DeviceType = Sink | Source deriving (Generic, Show)
data DeviceInfo = DeviceInfo
        { deviceType :: DeviceType
        , deviceIndex :: Int
        , deviceName :: String
        } deriving (Generic, Show) 
data StreamType = SinkInput | SourceOutput deriving (Generic, Show)
data StreamInfo = StreamInfo
        { streamType :: StreamType
        , streamIndex :: Int
        , applicationName :: Maybe String
        , applicationbinaryName :: Maybe String
        , verbose :: String
        , assignedDevice :: Either Int String
        } deriving (Generic, Show) 


twoMaybesToEither :: Maybe Int -> Maybe b -> Either Int b
twoMaybesToEither Nothing Nothing = Left $ negate 1
twoMaybesToEither (Just a) _ = Left a
twoMaybesToEither _ (Just b) = Right b 

instance FromJSON StreamInfo where
  parseJSON (Object v) = do
    index <- v .: "index"
    appName <- explicitParseField (\(Object p) -> p .:? "application.name") v "properties"
    appBinName <- explicitParseField (\(Object p) -> p .:? "application.process.binary") v "properties"
    verbose <- explicitParseField (\(Object p) -> p .: "module-stream-restore.id") v "properties"
    devicePlaying <- v .:? "sink"
    deviceRecieving <- explicitParseField (\(Object p) -> p .:? "target.object") v "properties"
    pure $ StreamInfo SinkInput index appName appBinName verbose (twoMaybesToEither devicePlaying deviceRecieving)


instance FromJSON DeviceInfo where
  parseJSON (Object v) = do
    i <- v .: "index"
    n <- v .: "name"
    pure $ DeviceInfo Sink i n
