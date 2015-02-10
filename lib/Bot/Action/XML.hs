{-# LANGUAGE OverloadedStrings, BangPatterns, DeriveDataTypeable #-}

module Bot.Action.XML
  ( readXML
  , value
  , elementsAt
  , children
  , mapElementsAt
  , valueAt
  , Path
  ) where

import Bot.Util
import Control.Monad.IO.Class
import Text.XML.Light

type Path = [Text]

readXML :: MonadIO m => FilePath -> m [Content]
readXML path = do
  !c <- liftIO $ readTextFile path
  return $ parseXML c

value :: Element -> Text
value e = pack $ strContent e

elementsAt :: Path -> [Content] -> [Element]
elementsAt [] _ = []
elementsAt (p:ps) cs = es
  where
    roots = filter ((== p) . pack . qName . elName) $ onlyElems cs
    es = atPath children roots ps
    
children :: Text -> Element -> [Element]
children n e = findChildren (qualName e n) e

qualName :: Element -> Text -> QName
qualName root name = blank_name { qName = unpack name
                                , qURI = qURI . elName $ root
                                }

mapElementsAt :: Path -> (Element -> b) -> [Content] -> [b]
mapElementsAt path f roots = map f $ elementsAt path roots
                     
valueAt :: Path -> Element -> [Text]
valueAt keys e = map pack $ map strContent $ atPath children [e] keys

atPath :: (k -> a -> [a]) -> [a] -> [k] -> [a]
atPath getChildren roots keys =
  foldl (\as k -> concatMap (getChildren k) as) roots keys

