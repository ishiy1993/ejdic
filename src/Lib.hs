{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Lens
import Data.Text.Lens
import Data.ByteString.Lens
import Network.Wreq

search :: String -> IO ()
search w = do
    let w' = w ^. packed
    let opt = defaults & params .~ [("Dic","EJdict"),("Word",w'),("Scope","HEADWORD"),("Match","STARTWITH"),("Merge","AND"),("Prof","XHTML"),("PageSize","20"),("PageIndex","0")]
    res <- getWith opt searchUrl
    putStrLn $ res ^. responseBody . unpackedChars

searchUrl :: String
searchUrl = "http://public.dejizo.jp/NetDicV09.asmx/SearchDicItemLite"

getUrl :: String
getUrl = "http://public.dejizo.jp/NetDicV09.asmx/GetDicItemLite"
