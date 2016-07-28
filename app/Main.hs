module Main where

import System.Environment (getArgs)
import System.IO

import Lib

main :: IO ()
main = do
    as <- getArgs
    case as of
      [] -> hPutStrLn stderr helpMsg
      (a:_) -> search a

helpMsg :: String
helpMsg = "Need one argument"
