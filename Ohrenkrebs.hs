module Ohrenkrebs where
import Lib
-- "doas modprobe snd_aloop"
-- "pactl list"
-- "pactl load-module module-loopback"
-- "pactl load-module module-null-sink"


name = "createdSinks"
sinkOp = openFile name ReadWriteMode

main = do
  sinkH <- sinkOp
  nullSink <- createProcessOut (proc "pactl" ["load-module", "module-null-sink"])
  listSinks <- createProcessOut (proc "pactl" ["list", "sinks"])
  listSources <- createProcessOut (proc "pactl" ["list", "sources"])
  listSinkInputs <- createProcessOut (proc "pactl" ["list", "sink-inputs"])
  listSourceOutputs <- createProcessOut (proc "pactl" ["list", "source-outputs"])
  putStr listSinkInputs
  hAppend sinkH nullSink hPutStr
  hClose sinkH
  pure ()
  

cleanup = do
  sinkH <- sinkOp
  openSinks <- lines <$> hGetContents sinkH
  forM_ openSinks (\sink -> putStrLn ("deleted " <> sink) >> createProcess (proc "pactl" ["unload-module", sink]))
  hClose sinkH
  clearFileWithName name


  
  
