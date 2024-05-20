module Ohrenkrebs where
import Lib


name = "createdSinks"
sinkOp = openFile name ReadWriteMode
execPactlOut flags = createProcessOut $ proc "pactl" flags
listJsonFlag name = ["-f", "json", "list", name]  

main = do
  sinkH <- sinkOp
  nullSink <- execPactlOut ["load-module", "module-null-sink"]
  s@[sinks, sources, sinkInputs, sinkOutputs] <- mapM (execPactlOut . listJsonFlag) $ words "sinks sources sink-inputs source-outputs"

  mapM_ (appendFile "parsingNeeded". (<> "\n")) s

  hAppend sinkH nullSink hPutStr
  hClose sinkH
  pure ()
  

cleanup = do
  sinkH <- sinkOp
  openSinks <- lines <$> hGetContents sinkH
  forM_ openSinks (\sink -> putStrLn ("deleted " <> sink) >> createProcess (proc "pactl" [ "unload-module", sink]))
  hClose sinkH
  clearFileWithName name


  
  
