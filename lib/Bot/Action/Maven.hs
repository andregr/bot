{-# LANGUAGE OverloadedStrings #-}

module Bot.Action.Maven
  ( maven
  , version
  , properties
  ) where

import Bot.Action.Action
import Bot.Action.XML
import Bot.Types
import Bot.Util
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Text.XML.Light as X
import System.Directory

maven :: Text -> Project -> Action
maven cmd project = do
  let path = projectPath project
  isMavenProject <- liftIO $ doesFileExist $ path ++ "/pom.xml"
  unless isMavenProject $ do
    throwA $ "Not a project (pom.xml not found): {}" % T.pack path
  silentProjectCommand ("mvn {}" % cmd) project
  liftIO $ putStrLn "ok"

version :: Project -> Action
version project = do
  pom <- readXML (projectPath project ++ "/pom.xml")
  -- TODO
  let vs = map value $ elementsAt ["project", "version"] pom
  case vs of
    []  -> throwA "Couldn't find /project/version"
    [v] -> liftIO $ T.putStrLn v
    _   -> throwA "Found multiple /project/version"

properties :: Project -> Action
properties project = do
  pom <- readXML (projectPath project ++ "/pom.xml")
  -- TODO
  let es = elementsAt (T.words "project properties") pom
      ps = concatMap X.elChildren es
      vs = map (T.pack . X.qName . X.elName &&& value) ps
      
  liftIO $ do
    T.putStrLn ""
    mapM_ (\(n,v) -> T.putStrLn . indent 1 $ "{}: {}" %% (n, v)) vs
