{-# LANGUAGE OverloadedStrings #-}

module Bot.Util 
  (
  -- * Formatting
    Text
  , null
  , T.pack
  , T.unpack
  , readTextFile
  , writeTextFile
  , appendTextFile
  , LT.fromStrict
  , LT.toStrict
  , Only(..)
  , (%)
  , (%%)
  , printf
  , putf
  , commas
  , indent
  , leftAlign
  , rightAlign
  , checkMarkChar
  , xMarkChar
  , C.Color(..)
  , shellColor
  ) where

import Control.Monad.IO.Class          ( MonadIO )
import Data.Monoid
import Data.Text                       ( Text )
import qualified Data.Text as T
import Data.Text.Buildable             ( Buildable )
import Data.Text.Format                ( Format, Only(..), format )
import qualified Data.Text.Format as T ( print )
import Data.Text.Format.Params         ( Params )
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified System.Console.ANSI as C

-- Formatting
-------------------------------------------------

(%) :: Buildable a => Format -> a -> Text
f % t = f %% (Only t)
infixl 9 %


(%%) :: Params ps => Format -> ps -> Text
f %% ps = LT.toStrict $ format f ps
infixl 9 %%

putf :: (MonadIO m, Params ps) => Format -> ps -> m ()
putf = T.print

printf :: (MonadIO m, Params ps) => Format -> ps -> m ()
printf f ps = putf (f <> "\n") ps

commas :: [Text] -> Text
commas ts = T.intercalate ", " ts

indent :: Int -> Text -> Text
indent i t = (T.replicate i  "    ") <> t

rightAlign :: Int -> Text -> Text
rightAlign s t = T.replicate (s - T.length t) " " <> t

leftAlign :: Int -> Text -> Text
leftAlign s t = t <> T.replicate (s - T.length t) " "

readTextFile :: FilePath -> IO Text
readTextFile = T.readFile

writeTextFile :: FilePath -> Text -> IO ()
writeTextFile = T.writeFile

appendTextFile :: FilePath -> Text -> IO ()
appendTextFile = T.appendFile

checkMarkChar, xMarkChar :: Text
checkMarkChar = "\10004"
xMarkChar = "\10008"

shellColor :: C.Color -> Text -> Text
shellColor c t = T.pack (C.setSGRCode [C.SetColor C.Foreground C.Vivid c])
              <> t
              <> T.pack (C.setSGRCode [C.Reset])
