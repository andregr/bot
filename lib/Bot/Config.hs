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
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.IO as ST -- strict IO to avoid locks from multiple calls
import System.Directory

defaultConfigFile :: FilePath
defaultConfigFile = "/home/andregr/.botDefaultConfig"

defaultConfiguration :: IO Text
defaultConfiguration = do
  fileExists <- doesFileExist defaultConfigFile
  if fileExists
    then head . T.lines . T.fromStrict<$> ST.readFile defaultConfigFile
    else return (configName . head $ configurations)

configurations :: [Configuration]
configurations = [ makeConfiguration "com" comWorkspace
                 , makeConfiguration "gen" genWorkspace
                 , makeConfiguration "releaser" releaserWorkspace
                 , makeConfiguration "tproc" tprocWorkspace
                 , makeConfiguration "test" testWorkspace
                 , makeConfiguration "tprop" tpropWorkspace
                 ]

makeConfiguration :: Text -> [Project] -> Configuration
makeConfiguration name projects = Configuration name commands help
  where
    gitOk c p = git c p >> (liftIO $ T.putStrLn "ok")
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
            forEachProject fetch
            <$> workspaceProjects

      , Command "deleteBranches" $
            forEachProject deleteBranches
            <$> workspaceProjects

      , Command "nuke" $
            forEachProject (\p -> nuke p >> (liftIO $ T.putStrLn "ok"))
            <$> workspaceProjects

      , Command "pull" $
            forEachProject2 gitOk
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
            forEachProject2' properties
            <$> workspaceProjects
            <*> arg "[matching]" maybeGrepPropertyParser

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

comWorkspace :: [Project]
comWorkspace = projects
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

genWorkspace :: [Project]
genWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace"
    projects = map (wsProject workspace)
      [ "comercial"
      , "faturamento"
      , "velab-comercial"
      , "atendimento"
      , "genesis"
      , "velab-genesis"
      , "motion-lis-genesis"
      ]

tprocWorkspace :: [Project]
tprocWorkspace = projects
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

tpropWorkspace :: [Project]
tpropWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace/"
    projects = map (wsProject workspace)
      [ "touch-property"
      , "bi-temporal"
      , "beta"
      , "classificacao"
      , "modifiers"
      , "beta-temporal"
      ]


wsProject :: FilePath -> Text -> Project
wsProject ws name = Project name (ws ++ "/" ++ T.unpack name)

configure :: Action
configure = do
  let bot = "/home/andregr/Code/bot"
  bashInteractive $ "emacs {}/lib/Bot/Config.hs" % bot
  liftIO $ putStrLn "Recompiling"
  output <- bash $ "cd {} && /home/andregr/.cabal/bin/cabal build" % bot
  liftIO $ T.putStrLn output  

changeConfig :: Text -> Action
changeConfig config = do
  unless (config `elem` map configName configurations) $
    throwA $ "Unknown configuration '{}'" % config
  liftIO $ ST.writeFile defaultConfigFile $ T.toStrict config

maybeGrepPropertyParser :: Parser (Maybe ((Text, Text) -> Bool))
maybeGrepPropertyParser = fmap (fmap grep) $ optional text
  where
    grep :: Text -> (Text, Text) -> Bool
    grep s (n, v) = or $ map (s `T.isInfixOf`) [n, v]
