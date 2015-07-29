{-# LANGUAGE OverloadedStrings #-}

module Bot.Action.Maven
  ( maven
  , version
  , parentVersion
  , changeDependencyVersion
  , updateDependencyVersions
  , properties
  , snapshots
  ) where

import Bot.Action.Action
import Bot.Action.XML
import Bot.Types
import Bot.Util
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Data.List (inits)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.XML.Light as X
import System.Directory

maven :: Text -> [(Text, [Text])] -> Project -> Action
maven cmd projectProfiles project = do
  let path = projectPath project
  isMavenProject <- liftIO $ doesFileExist $ path ++ "/pom.xml"
  unless isMavenProject $ do
    throwA $ "Not a project (pom.xml not found): {}" % pack path
  let profiles = case lookup (projectName project) projectProfiles of
        Nothing -> ""
        Just ps -> "-P" <> (T.intercalate "," ps)
  output <- silentProjectCommand ("mvn {} {}" %% (profiles, cmd)) project
  liftIO $
    if "BUILD FAILURE" `T.isInfixOf` output
      then do
        T.putStrLn $ red xMarkChar <> ". Command implicitly failed with 'BUILD FAILURE' log"
        showOutput output
      else T.putStrLn $ green checkMarkChar

version :: Project -> Action
version project = projectVersion project >>= (liftIO . T.putStrLn)

projectVersion :: Project -> ActionM Text
projectVersion project = do
  pom <- readPOM project
  readSingleValue pom ["project", "version"]

parentVersion :: Project -> Action
parentVersion project = do
  pom <- readPOM project
  name <- readSingleValue pom ["project", "parent", "artifactId"]
  version <- readSingleValue pom ["project", "parent", "version"]
                   
  liftIO $ printf "{}: {}" (name, version)

changeDependencyVersion :: Project -> Text -> Text -> Action
changeDependencyVersion project depName newVersion = do
  let pomPath = projectPath project ++ "/pom.xml"
      openTag = "<touch.{}.version>" % depName
      closeTag = "</touch.{}.version>" % depName
  void $ bash ("sed 's|\\(.*{}\\).*\\({}.*\\)|\\1{}\\2|g' -i {}" %% (openTag, closeTag, newVersion, pomPath))

updateDependencyVersions :: [Project] -> Action
updateDependencyVersions projects = do
    let steps = tail $ inits projects
    forM_ (zip [0..] steps) $ \(i, s) -> do
      changeVersionsAction s (projects !! i)
  
  where
    changeVersionsAction :: [Project] -> Project -> Action
    changeVersionsAction ps t = forM_ ps $ \p -> do
      let pName = projectName p
      pVersion <- projectVersion p
      changeDependencyVersion t pName pVersion

properties :: Project -> Maybe ((Text, Text) -> Bool) -> Action
properties project mf = do
  pom <- readPOM project
  ps <- readProperties pom

  let f = maybe (const True) id mf
      vs = filter f ps

  liftIO $ do
    putStrLn ""
    mapM_ (\(n,v) -> T.putStrLn . indent 1 $ "{}: {}" %% (n, v)) vs

snapshots :: Project -> Action
snapshots project = do
  pom <- readPOM project
  props <- readProperties pom
  parentName <- readSingleValue pom ["project", "parent", "artifactId"]
  parentVersion <- readSingleValue pom ["project", "parent", "version"]
  let ps = ((parentName, parentVersion):props)

  let f (_, v) = "SNAPSHOT" `T.isInfixOf` v
      vs = filter f ps

  liftIO $ do
    putStrLn ""
    mapM_ (\(n,v) -> T.putStrLn . indent 1 $ "{}: {}" %% (n, v)) vs      

readProperties :: [X.Content] -> ActionM [(Text, Text)]
readProperties rs = do
  let path = ["project", "properties"]
      prop = pack . X.qName . X.elName &&& value
      ps = mapElementsAt path (map prop . X.elChildren) rs
  case ps of
    [] -> throwA $ "Couldn't find /{}" % T.intercalate "/" path
    [p] -> return p
    _   -> throwA $ "Found multiple /{}" % T.intercalate "/" path
  
readSingleValue :: [X.Content] -> Path -> ActionM Text
readSingleValue rs path = do
  let vs = mapElementsAt path value rs
  case vs of
    []  -> throwA $ "Couldn't find /{}" % T.intercalate "/" path
    [v] -> return v
    _   -> throwA $ "Found multiple /{}" % T.intercalate "/" path

readPOM :: Project -> ActionM [X.Content]
readPOM project = do
  let pom = projectPath project ++ "/pom.xml"
  pomExists <- liftIO $ doesFileExist pom
  unless pomExists $
    throwA $ "pom doesn't exist at '{}'" % pack pom
  readXML pom
