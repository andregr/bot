{-# LANGUAGE OverloadedStrings #-}

module Bot.Config
  ( defaultConfiguration
  , configurations
  ) where

import Bot.Action.Action
import Bot.Action.Git
import Bot.Action.Maven
import Bot.Parser.Parser
import Bot.Parser.Project
import Bot.Types
import Bot.Util
import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as T
import qualified Data.Text.IO as T -- strict IO to avoid locks from multiple calls
import System.Directory

defaultConfigFile :: FilePath
defaultConfigFile = "/home/andregr/.botDefaultConfig"

defaultConfiguration :: IO Text
defaultConfiguration = do
  fileExists <- doesFileExist defaultConfigFile
  if fileExists
    then head . T.lines . T.fromStrict<$> T.readFile defaultConfigFile
    else return (configName . head $ configurations)

configurations :: [Configuration]
configurations = [ makeConfiguration "real" realWorkspace
                 , makeConfiguration "releaser" releaserWorkspace
                 , makeConfiguration "tiss" tissWorkspace
                 , makeConfiguration "test" testWorkspace                   
                 ]

makeConfiguration :: Text -> [Project] -> Configuration
makeConfiguration name projects = Configuration name commands help
  where
    commands =
      [ Command "configure" $
            pure configure

      , Command "changeConfig" $
            changeConfig
            <$> arg "config" text

      , Command "reinstall" $
            forEachProject2 maven
            <$> pure "-DskipTests=true clean install"
            <*> workspaceProjects
          
      , Command "install" $
            forEachProject2 maven
            <$> pure "-DskipTests=true install"
            <*> workspaceProjects
          
      , Command "status" $
            forEachProject status
            <$> workspaceProjects

      , Command "fetch" $
            forEachProject2 git
            <$> pure "fetch"
            <*> workspaceProjects

      , Command "deleteBranches" $
            forEachProject deleteBranches
            <$> workspaceProjects

      , Command "nuke" $
            forEachProject nuke
            <$> workspaceProjects

      , Command "pull" $
            forEachProject2 git
            <$> pure "pull"
            <*> workspaceProjects
          
      , Command "createBranch" $
            forEachProject2 createBranch
            <$> arg "branch" text
            <*> workspaceProjects
          
      , Command "version" $
            forEachProject version
            <$> workspaceProjects

      , Command "properties" $
            forEachProject properties
            <$> workspaceProjects

      , Command "bash" $
            forEachProject2 bashAction
            <$> arg "command" text
            <*> workspaceProjects
      ]
    
    workspaceProjects = arg "projects" $ projectsParser projects

    help =    [ "Commands:" ]
           ++ [ "" ]
           ++ concatMap (fmap (indent 1) . showHelp) commands
           ++ [ "" ]
           ++ [ "Projects:" ]
           ++ [ "" ]
           ++ map (indent 1 . projectName) projects

realWorkspace :: [Project]
realWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace"
    projects = map (wsProject workspace)
      [ "financeiro"
      , "cobranca-api"
      , "geradorrps"
      , "comercial"
      , "faturamento"
      , "bpa-comercial"
      , "autorizacao"
      , "velab-comercial"
      , "tiss-comercial"
      , "comercial-lis"
      ]

tissWorkspace :: [Project]
tissWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace"
    projects = map (wsProject workspace)
      [ "touch-protocol"
      , "integracao-comercial"
      , "comercial"
      , "faturamento"
      , "comercial-lis"
      ]

releaserWorkspace :: [Project]
releaserWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace/releaser/test/releaser_workspace"
    projects = map (wsProject workspace)
      [ "release-test-root"
      , "release-test-alpha"
      , "release-test-beta"
      , "release-test-charlie"
      ]

testWorkspace :: [Project]
testWorkspace = projects
  where
    workspace = "/Users/andre/Code/bot/test/data"
    projects = map (wsProject workspace)
      [ "my-app"
      , "my-app2"
      , "my-app3"
      , "my-app4"
      , "my-app5"
      ]

wsProject :: FilePath -> Text -> Project
wsProject ws name = Project name (ws ++ "/" ++ T.unpack name)

configure :: Action
configure = do
  let bot = "/home/andregr/Code/bot"
  bashInteractive $ "emacs {}/lib/Bot/Config.hs" % bot
  liftIO $ putStrLn "Recompiling"
  output <- bash $ "cd {} && /home/andregr/.cabal/bin/cabal install" % bot
  liftIO $ putStrLn output  

changeConfig :: Text -> Action
changeConfig config = liftIO $ T.writeFile defaultConfigFile $ T.toStrict config
