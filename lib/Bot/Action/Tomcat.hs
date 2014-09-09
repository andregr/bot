{-# LANGUAGE OverloadedStrings #-}

module Bot.Action.Tomcat where

import Bot.Action.Action
import Bot.Action.Maven
import Bot.Types
import Bot.Util
import Control.Applicative
import Data.Monoid
import qualified Data.Text.Lazy as T
import System.FilePath.Posix

deploy :: Project -> Action
deploy project = do
  undefined

deployableArtifactPath :: Project -> ActionM FilePath
deployableArtifactPath project = do
  m <- deployableModulePath $ projectPath project
  v <- version project
  return $ projectPath project </> m </> undefined

deployableModulePath :: FilePath -> ActionM FilePath
deployableModulePath projPath = do
  files <- findProjectSourceFiles projPath "struts.xml"
  f <- case files of
    [f] -> return f
    [] -> throwA "Couldn't find struts.xml"
    _  -> throwA "Found multiple struts.xml"
  -- TODO: fail if "src" not found
  let p = (concat . takeWhile (/= "src") . splitPath) f
  return p

findProjectSourceFiles :: FilePath -> Text -> ActionM [FilePath]
findProjectSourceFiles projPath name = do
  let findCmd = ("find -not -path '\''*/target/*'\'' "
              <> "-not -path '\''*/bin/*'\'' "
              <> "-name '{}'") % name
  (lines . T.unpack) <$>  cd projPath (bash findCmd)  
