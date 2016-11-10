{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Lens
import Data.Text.Lens hiding (text)
import Data.ByteString.Lens
import System.IO
import Text.XML
import Text.XML.Lens
import Network.Wreq
import Network.HTTP.Client (HttpException)

search :: String -> IO ()
search w = do
    let w' = w ^. packed
    let opt = defaults & params .~ [("Dic","EJdict")
                                   ,("Word",w')
                                   ,("Scope","HEADWORD")
                                   ,("Match","EXACT")
                                   ,("Merge","AND")
                                   ,("Prof","XHTML")
                                   ,("PageSize","20")
                                   ,("PageIndex","0")
                                   ]
    res <- getWith opt searchUrl
    let doc = parseLBS_ def $ res ^. responseBody
        n = doc ^. root . name
        nTitleList = n & _nameLocalName .~ "TitleList"
        nDicItemTitle = n & _nameLocalName .~ "DicItemTitle"
        nItemID = n & _nameLocalName .~ "ItemID"
        nTitle = n & _nameLocalName .~ "Title"
        ws = doc ^.. root . el n 
                          ./ el nTitleList
                          ./ el nDicItemTitle
                          ./ el nTitle
                          ./ el "span"
                          . text
        is = doc ^.. root . el n
                          ./ el nTitleList
                          ./ el nDicItemTitle
                          ./ el nItemID
                          . text
        ls =  zip ws is
    case lookup w' ls of
      Nothing -> putErr "Not Found"
      Just id -> do
        let gOpt = defaults & params .~ [("Dic","EJdict")
                                        ,("Item",id)
                                        ,("Loc","")
                                        ,("Prof","XHTML")
                                        ]
        res2 <- getWith gOpt getUrl
        let doc2 = parseLBS_ def $ res2 ^. responseBody
            n2 = doc2 ^. root . name
            nHead = n2 & _nameLocalName .~ "Head"
            nBody = n2 & _nameLocalName .~ "Body"
            h = head $ doc2 ^.. root . el n2
                                     ./ el nHead
                                     ./ el "div"
                                     ./ el "span"
                                     . text
            b = head $ doc2 ^.. root . el n2
                                     ./ el nBody
                                     ./ el "div"
                                     ./ el "div"
                                     . text
        putStrLn $ h ^. unpacked
        putStrLn $ b ^. unpacked

errHandle :: HttpException -> IO ()
errHandle e = putErr $ show e

putErr :: String -> IO ()
putErr = hPutStrLn stderr

searchUrl :: String
searchUrl = "http://public.dejizo.jp/NetDicV09.asmx/SearchDicItemLite"

getUrl :: String
getUrl = "http://public.dejizo.jp/NetDicV09.asmx/GetDicItemLite"
