{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module AudioBuster where
import Lib
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
        , applicationName :: String
        , applicationbinaryName :: String
        -- , assignedDevice :: DeviceInfo
        } deriving (Generic, Show) 

instance FromJSON StreamInfo where
  parseJSON (Object v) = do
    index <- v .: "index"
    appName <- explicitParseField (\(Object p) -> p .: "application.name") v "properties"
    appBinName <- explicitParseField (\(Object p) -> p .: "application.process.binary") v "properties"
    -- <*> explicitParseFieldMaybe (\(Object p) -> p .: "target.object") v "properties"
    -- <*> v .: "Sink"
    pure $ StreamInfo SinkInput index appName appBinName


instance FromJSON DeviceInfo where
  parseJSON (Object v) = do
    i <- v .: "index"
    n <- v .: "name"
    pure $ DeviceInfo Sink i n
  

name = "createdSinks"
sinkOp = openFile name ReadWriteMode
execPactlOut flags = createProcessOut $ proc "pactl" flags
listJsonFlag name = ["-f", "json", "list", name]  

main = do
  -- sinkH <- sinkOp
  -- nullSink <- execPactlOut ["load-module", "module-null-sink"]

  s@[sinks, sources, sinkInputs, sourceOutputs] <- mapM (execPactlOut . listJsonFlag) $ words "sinks sources sink-inputs source-outputs"

  mapM_ (appendFile "parsingNeeded". (<> "\n")) s

  let
    parsedSinks = fromJust (decode (fromString sinks) :: Maybe [DeviceInfo])
    parsedSources = fromJust (decode (fromString sources) :: Maybe [DeviceInfo])
    parsedSinkInputs = fromJust (decode (fromString sinkInputs) :: Maybe [StreamInfo])
    parsedSourceOutputs = fromJust (decode (fromString sourceOutputs) :: Maybe [StreamInfo])
  -- putStr new
  print $ replicate 25 '-' <> " Sinks " <> replicate 25 '-'
  mapM_ print parsedSinks
  print $ replicate 25 '-' <> " Sources " <> replicate 25 '-'
  mapM_ print parsedSources
  print $ replicate 25 '-' <> " Sink Inputs " <> replicate 25 '-'
  mapM_ print parsedSinkInputs
  print $ replicate 25 '-' <> " Source Outputs " <> replicate 25 '-'
  mapM_ print parsedSourceOutputs

  -- hAppend sinkH nullSink hPutStr
  -- hClose sinkH
  pure ()
  

cleanup = do
  sinkH <- sinkOp
  openSinks <- lines <$> hGetContents sinkH
  forM_ openSinks (\sink -> putStrLn ("deleted " <> sink) >> createProcess (proc "pactl" [ "unload-module", sink]))
  hClose sinkH
  clearFileWithName name
