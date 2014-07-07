{-# LANGUAGE OverloadedStrings, BangPatterns, DeriveDataTypeable #-}

module Bot.Action.XML
  ( readXML
  , value
  , elementsAt
  , children
  ) where

import Control.Monad.IO.Class
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Text.XML.Light

readXML :: MonadIO m => FilePath -> m [Content]
readXML path = do
  !c <- liftIO $ T.readFile path
  return $ parseXML c

value :: Element -> Text
value e = T.pack $ strContent e

elementsAt :: [Text] -> [Content] -> [Element]
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

atPath :: (k -> a -> [a]) -> [a] -> [k] -> [a]
atPath getChildren roots keys =
  foldl (\as k -> concatMap (getChildren k) as) roots keys
