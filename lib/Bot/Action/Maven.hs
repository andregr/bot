{-# LANGUAGE OverloadedStrings #-}

module Bot.Action.Maven where

import Bot.Action.Action
import Bot.Types
import Bot.Util
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as T
import System.Directory

maven :: Text -> Project -> IO ()
maven cmd project = do
  let path = projectPath project
  isMavenProject <- liftIO $ doesFileExist $ path ++ "/pom.xml"
  unless isMavenProject $ do
    throwIO $ ActionException $ "Not a project (pom.xml not found): {}" % T.pack path
  silentProjectCommand ("mvn {}" % cmd) project
