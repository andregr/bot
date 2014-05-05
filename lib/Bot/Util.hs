module Bot.Util 
  (
  -- * Formatting
    Text
  , Only(..)
  , (%)
  , printf

  ) where

import Control.Monad.IO.Class          ( MonadIO )
import Data.Text.Format                ( Format, Only(..), format )
import qualified Data.Text.Format as T ( print )
import Data.Text.Format.Params         ( Params )
import Data.Text.Lazy                  ( Text, pack )

-- Formatting
-------------------------------------------------

(%) :: Params ps => Format -> ps -> Text
(%) = format
infix 9 %

printf :: (MonadIO m, Params ps) => Format -> ps -> m ()
printf = T.print