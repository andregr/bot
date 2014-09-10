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
           ++ map (indent 1 . (\p -> "{} ({})" %% [projectName p, commas $ projectAliases p])) projects

comWorkspace :: [Project]
comWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace"
    projects = map (wsProject workspace)
      [ ("financeiro", ["fin"])
      , ("cobranca-api", ["cob"])
      , ("geradorrps", ["ger"])
      , ("comercial", ["com"])
      , ("faturamento", ["fat"])
      , ("bpa-comercial", ["bpa"])
      , ("autorizacao", ["auto"])
      , ("velab-comercial", ["vcom"])
      , ("tiss-comercial", ["tiss"])
      , ("comercial-lis", ["lis"])
      ]

genWorkspace :: [Project]
genWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace"
    projects = map (wsProject workspace)
      [ ("comercial", ["com"])
      , ("faturamento", ["fat"])
      , ("velab-comercial", ["vcom"])
      , ("atendimento", ["ate"])
      , ("genesis", [])
      , ("velab-genesis", ["vgen"])
      , ("motion-lis-genesis", ["gen"])
      ]

tprocWorkspace :: [Project]
tprocWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace"
    projects = map (wsProject workspace)
      [ ("touch-protocol", ["prot"])
      , ("integracao-comercial", ["int"])
      , ("cobranca-api", ["cob"])
      , ("comercial", ["com"])
      , ("faturamento", ["fat"])
      , ("velab-comercial", ["vcom"])
      , ("comercial-lis", ["lis"])
      ]

releaserWorkspace :: [Project]
releaserWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace/releaser/test/releaser_workspace"
    projects = map (wsProject workspace)
      [ ("release-test-root", ["root"])
      , ("release-test-alpha", ["alpha"])
      , ("release-test-beta", ["beta"])
      , ("release-test-charlie", ["charlie"])
      ]

testWorkspace :: [Project]
testWorkspace = projects
  where
    workspace = "/Users/andre/Code/bot/test/data"
    projects = map (wsProject workspace)
      [ ("my-app", [])
      , ("my-app2", [])
      , ("my-app3", [])
      , ("my-app4", [])
      , ("my-app5", [])
      ]

tpropWorkspace :: [Project]
tpropWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace/"
    projects = map (wsProject workspace)
      [ ("touch-property", ["tprop"])
      , ("bi-temporal", ["bi"])
      , ("beta", ["bta"])
      , ("classificacao", ["class"])
      , ("modifiers", ["mod"])
      , ("beta-temporal", ["bbt"])
      ]


wsProject :: FilePath -> (Text, [Text]) -> Project
wsProject ws (name, aliases) = Project name (ws ++ "/" ++ T.unpack name) aliases

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
