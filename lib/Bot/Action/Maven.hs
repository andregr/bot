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
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Text.XML.Light as X
import System.Directory

maven :: Text -> Project -> IO ()
maven cmd project = do
  let path = projectPath project
  isMavenProject <- liftIO $ doesFileExist $ path ++ "/pom.xml"
  unless isMavenProject $ do
    throwIO $ ActionException $ "Not a project (pom.xml not found): {}" % T.pack path
  silentProjectCommand ("mvn {}" % cmd) project

version :: Project -> IO ()
version project = do
  pom <- readXML (projectPath project ++ "/pom.xml")
  -- TODO
  let vs = map value $ elementsAt ["project", "version"] pom
  case vs of
    []  -> throwIO $ ActionException $ "Couldn't find /project/version"
    [v] -> T.putStrLn v
    _   -> throwIO $ ActionException $ "Found multiple /project/version"

properties :: Project -> IO ()
properties project = do
  pom <- readXML (projectPath project ++ "/pom.xml")
  -- TODO
  let es = elementsAt (T.words "project properties") pom
      ps = concatMap X.elChildren es
      vs = map (T.pack . X.qName . X.elName &&& value) ps
      
  T.putStrLn ""
  mapM_ (\(n,v) -> T.putStrLn . indent 1 $ "{}: {}" %% (n, v)) vs
