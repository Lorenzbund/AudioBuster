{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module AudioBuster where
import Text.Printf
import Control.Monad
import Data.Function

import Lib
import Parsing

name = "createdSinks"
sinkOp = openFile name ReadWriteMode
execPactlOut flags = createProcessOut $ proc "pactl" flags
listJsonFlag name = ["-f", "json", "list", name]  

main = do
  sinkH <- sinkOp
  _nullSink <- execPactlOut ["load-module", "module-null-sink"]
  hAppend sinkH _nullSink hPutStr
  hClose sinkH
  s@[sinks, sources, sinkInputs, sourceOutputs] <- mapM (execPactlOut . listJsonFlag) $ words "sinks sources sink-inputs source-outputs"

  -- mapM_ (appendFile "parsingNeeded". (<> "\n")) s

  let
    parseString :: FromJSON a => String -> a
    parseString = fromJust . decode . fromString
    devices@[parsedSinks, parsedSources] = (map parseString [sinks, sources] :: [[DeviceInfo]]) & \(x:y:xs) -> x : map (\y' -> y' { deviceType = Source }) y : xs
    streams@[parsedSinkInputs, parsedSourceOutputs] = (map parseString [sinkInputs, sourceOutputs] :: [[StreamInfo]]) & \(x:y:xs) ->  x : map (\y' -> y' { streamType = SourceOutput }) y : xs

  print $ replicate 25 '-' <> " Sinks " <> replicate 25 '-'
  mapM_ print parsedSinks
  print $ replicate 25 '-' <> " Sources " <> replicate 25 '-'
  mapM_ print parsedSources
  print $ replicate 25 '-' <> " Sink Inputs " <> replicate 25 '-'
  mapM_ print parsedSinkInputs
  print $ replicate 25 '-' <> " Source Outputs " <> replicate 25 '-'
  mapM_ print parsedSourceOutputs


  nullSink <- getLine
  printf "{- Sink Inputs -} Input stream Index: "
  let
    readI = readLn :: IO Int
  sinkInd <- show <$> readI
  printf "{- Source Outputs -} Application to recieve Input: " 
  sourceInd <- show <$> readI

  void $ execPactlOut ["move-sink-input", sinkInd, nullSink]
  void $ execPactlOut ["move-source-output", sourceInd, nullSink]

  
  pure ()
  

cleanup = do
  sinkH <- sinkOp
  openSinks <- lines <$> hGetContents sinkH
  forM_ openSinks (\sink -> putStrLn ("deleted " <> sink) >> createProcess (proc "pactl" [ "unload-module", sink ]))
  hClose sinkH
  clearFileWithName name
