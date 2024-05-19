module Lib
  ( module System.Process
  , module System.IO
  , module Control.Monad
  , clearFileWithName
  , createProcessOut
  , hAppend
  )
where

import System.Process
import System.IO
import Control.Monad
clearFileWithName :: FilePath -> IO ()
clearFileWithName name = do
  writeFile name ""

createProcessOut :: CreateProcess -> IO String
createProcessOut cp = do
  (_, Just out, _ , _) <-createProcess cp { std_out = CreatePipe }
  hGetContents out

hAppend :: Handle -> String -> (Handle -> String -> IO a) -> IO a
hAppend h str f = do
  hSeek h SeekFromEnd 0
  f h str
