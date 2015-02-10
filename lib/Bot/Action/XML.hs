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

import Control.Monad.IO.Class
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Text.XML.Light

type Path = [Text]

readXML :: MonadIO m => FilePath -> m [Content]
readXML path = do
  !c <- liftIO $ T.readFile path
  return $ parseXML c

value :: Element -> Text
value e = T.pack $ strContent e

elementsAt :: Path -> [Content] -> [Element]
elementsAt [] _ = []
elementsAt (p:ps) cs = es
  where
    roots = filter ((== p) . T.pack . qName . elName) $ onlyElems cs
    es = atPath children roots ps
    
children :: Text -> Element -> [Element]
children n e = findChildren (qualName e n) e

qualName :: Element -> Text -> QName
qualName root name = blank_name { qName = T.unpack name
                                , qURI = qURI . elName $ root
                                }

mapElementsAt :: Path -> (Element -> b) -> [Content] -> [b]
mapElementsAt path f roots = map f $ elementsAt path roots
                     
valueAt :: Path -> Element -> [Text]
valueAt keys e = map T.pack $ map strContent $ atPath children [e] keys

atPath :: (k -> a -> [a]) -> [a] -> [k] -> [a]
atPath getChildren roots keys =
  foldl (\as k -> concatMap (getChildren k) as) roots keys

