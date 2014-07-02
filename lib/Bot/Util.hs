{-# LANGUAGE OverloadedStrings #-}

module Bot.Util 
  (
  -- * Formatting
    Text
  , Only(..)
  , (%)
  , (%%)
  , printf
  , putf
  , commas
  , indent
  , leftAlign
  , rightAlign
  ) where

import Control.Monad.IO.Class          ( MonadIO )
import Data.Int                        ( Int64 )
import Data.Monoid
import Data.Text.Format                ( Format, Only(..), format )
import qualified Data.Text.Format as T ( print )
import Data.Text.Format.Params         ( Params )
import Data.Text.Lazy                  ( Text )
import qualified Data.Text.Lazy as T

-- Formatting
-------------------------------------------------

(%) :: Format -> Text -> Text
f % t = f %% (Only t)
infixl 9 %


(%%) :: Params ps => Format -> ps -> Text
(%%) = format
infixl 9 %%

putf :: (MonadIO m, Params ps) => Format -> ps -> m ()
putf = T.print

printf :: (MonadIO m, Params ps) => Format -> ps -> m ()
printf f ps = putf (f <> "\n") ps

commas :: [Text] -> Text
commas ts = T.intercalate ", " ts

indent :: Int64 -> Text -> Text
indent i t = (T.replicate i  "    ") <> t

rightAlign :: Int64 -> Text -> Text
rightAlign s t = T.replicate (s - T.length t) " " <> t

leftAlign :: Int64 -> Text -> Text
leftAlign s t = t <> T.replicate (s - T.length t) " "
