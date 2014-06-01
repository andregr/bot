{-# LANGUAGE OverloadedStrings #-}

module Bot.Config where

import Control.Applicative ((<$>))
import Bot.Parser (arg, text)
import Bot.Types (Configuration(..), Command(..), Action, Result(..))
import Bot.Util (printf, Only(..))
import Data.Text.Lazy (Text)

configuration :: Configuration
configuration = Configuration $
  [ Command "welcome" (welcome <$> arg "name" text)
  ]

welcome :: Text -> Action
welcome name = printf "Welcome, {}!" (Only name) >> return Success