{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import Text.Printf
import Control.Monad
import Data.Function

import Options.Applicative
import Data.Foldable
import System.Directory

import Lib
import Parsing
data Commands = Init { initDir :: FilePath, filename :: String, createParents :: Bool }
              | List
              | Configure { sinkInput :: Int, sourceOutput :: Int, perBridge :: Int }
              | HardClean


documentation = fullDesc
commandsSubparser = subparser $ foldMap (uncurry command)
        [ (,) "init" initCommand
        , (,) "list" listCommand
        , (,) "configure" configureCommand
        , (,) "hclean" hardCleanupCommand
        ]

initCommand = info initParser documentation
  where
    initParser =
      Init
      <$> strOption (fold [long "dir", short 'd', value "./"])
      <*> strOption (fold [long "file", short 'f', value "createdSinks"])
      <*> flag True False (fold [short 'p']) 

listCommand = info (pure List) documentation
        where
          listParser = List
configureCommand = info configureParser documentation
        where
          configureParser = Configure
            <$> (option auto $ fold [long "insink", short 'i'])
            <*> (option auto $ fold [long "outsrc", short 'o'])
            <*> (option auto $ fold [long "via", short 'p'])
        
hardCleanupCommand = info (pure HardClean) documentation

name = "createdSinks"
sinkOp = openFile name ReadWriteMode
execPactlOut flags = createProcessOut $ proc "pactl" flags
listJsonFlag name = ["-f", "json", "list", name]

main = handleSubcommand =<< execParser (info commandsSubparser documentation)
handleSubcommand Init{..} = do
  createDirectoryIfMissing createParents initDir
  withCurrentDirectory initDir $ do
    sinkH <- openFile filename ReadWriteMode
    createdSink <- execPactlOut $ words "load-module module-null-sink"
    hAppend sinkH createdSink hPutStr
    hClose sinkH

handleSubcommand List = do
  s@[sinks, sources, sinkInputs, sourceOutputs] <- mapM (execPactlOut . listJsonFlag) $ words "sinks sources sink-inputs source-outputs"
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
  
handleSubcommand Configure{..} = do
  a <- execPactlOut $ words $ printf "move-sink-input %d %d" sinkInput perBridge
  b <- execPactlOut $ words $ printf "move-source-output %d %d" sourceOutput perBridge
  -- print a
  -- print b
  pure () 

handleSubcommand HardClean = do
  a <- execPactlOut $ words "unload-module module-null-sink"
  -- print a
  pure ()

cleanup = do
  sinkH <- sinkOp
  openSinks <- lines <$> hGetContents sinkH
  forM_ openSinks (\sink -> putStrLn ("deleted " <> sink) >> createProcess (proc "pactl" [ "unload-module", sink ]))
  hClose sinkH
  clearFileWithName name
