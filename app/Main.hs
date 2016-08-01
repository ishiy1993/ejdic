module Main where

import Control.Exception.Base
import System.Environment (getArgs)
import System.IO

import Lib

main :: IO ()
main = do
    as <- getArgs
    case as of
      [] -> hPutStrLn stderr helpMsg
      (a:_) -> search a `catch` errHandle

helpMsg :: String
helpMsg = "Need one argument"
