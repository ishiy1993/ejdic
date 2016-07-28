{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Lens
import Data.Text.Lens hiding (text)
import Data.ByteString.Lens
import Text.XML
import Text.XML.Lens
import Network.Wreq

search :: String -> IO ()
search w = do
    let w' = w ^. packed
    let opt = defaults & params .~ [("Dic","EJdict"),("Word",w'),("Scope","HEADWORD"),("Match","EXACT"),("Merge","AND"),("Prof","XHTML"),("PageSize","20"),("PageIndex","0")]
    res <- getWith opt searchUrl
    let doc = parseLBS_ def $ res ^. responseBody
        n = doc ^. root . name
        nTitleList = n & _nameLocalName .~ "TitleList"
        nDicItemTitle = n & _nameLocalName .~ "DicItemTitle"
        nItemID = n & _nameLocalName .~ "ItemID"
        nTitle = n & _nameLocalName .~ "Title"
        ws = doc ^.. root . el n ./ el nTitleList ./ el nDicItemTitle ./ el nTitle ./ el "span" . text
        is = doc ^.. root . el n ./ el nTitleList ./ el nDicItemTitle ./ el nItemID . text
    print $ zip ws is

searchUrl :: String
searchUrl = "http://public.dejizo.jp/NetDicV09.asmx/SearchDicItemLite"

getUrl :: String
getUrl = "http://public.dejizo.jp/NetDicV09.asmx/GetDicItemLite"
