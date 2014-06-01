{-# LANGUAGE OverloadedStrings #-}

module Bot.Util 
  (
  -- * Formatting
    Text
  , Only(..)
  , (%)
  , (%%)
  , printf
  , commas
  ) where

import Control.Monad.IO.Class          ( MonadIO )
import Data.Text.Format                ( Format, Only(..), format )
import qualified Data.Text.Format as T ( print )
import Data.Text.Format.Params         ( Params )
import Data.Text.Lazy                  ( Text )
import qualified Data.Text.Lazy as T   ( intercalate )

-- Formatting
-------------------------------------------------

(%) :: Format -> Text -> Text
f % t = f %% (Only t)
infixl 9 %


(%%) :: Params ps => Format -> ps -> Text
(%%) = format
infixl 9 %%

printf :: (MonadIO m, Params ps) => Format -> ps -> m ()
printf = T.print

commas :: [Text] -> Text
commas ts = T.intercalate ", " ts