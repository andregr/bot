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
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory

defaultConfigFile :: FilePath
defaultConfigFile = "/home/andregr/.botDefaultConfig"

defaultConfiguration :: IO Text
defaultConfiguration = do
  fileExists <- doesFileExist defaultConfigFile
  if fileExists
    then head . T.lines <$> readTextFile defaultConfigFile
    else return (configName . head $ configurations)

configurations :: [Configuration]
configurations = [ makeConfiguration "com" comWorkspace
                 , makeConfiguration "gen" genWorkspace
                 , makeConfiguration "mot" motWorkspace                   
                 , makeConfiguration "releaser" releaserWorkspace
                 , makeConfiguration "test" testWorkspace
                 , makeConfiguration "tprop" tpropWorkspace
                 , makeConfiguration "liq" liqWorkspace                   
                 ]

makeConfiguration :: Text -> [Project] -> Configuration
makeConfiguration name projects = Configuration name commands help
  where
    gitOk c p = git c p >> (liftIO $ putStrLn "ok")
    commands =
      [ Command "configure" ["conf"] $
            pure configure

      , Command "changeConfig" ["cc"] $
            changeConfig
            <$> arg "config" text

      , Command "generateScripts" [] $
            generateScripts
            <$> pure commands
            
      , Command "reinstall" ["ri"] $
            forEachProject2 maven
            <$> pure "-DskipTests=true clean install"
            <*> workspaceProjects
          
      , Command "install" ["i"] $
            forEachProject2 maven
            <$> pure "-DskipTests=true install"
            <*> workspaceProjects
            
      , Command "status" ["s"] $
            forEachProject status
            <$> workspaceProjects

      , Command "fetch" ["f"] $
            forEachProject fetch
            <$> workspaceProjects

      , Command "deleteBranches" [] $
            forEachProject deleteBranches
            <$> workspaceProjects

      , Command "nuke" [] $
            forEachProject (\p -> nuke p >> (liftIO $ putStrLn "ok"))
            <$> workspaceProjects

      , Command "pull" [] $
            forEachProject2 gitOk
            <$> pure "pull"
            <*> workspaceProjects
          
      , Command "createBranch" [] $
            forEachProject2 createBranch
            <$> arg "branch" text
            <*> workspaceProjects
          
      , Command "version" ["v"] $
            forEachProject version
            <$> workspaceProjects

      , Command "parentVersion" ["pv"] $
            forEachProject parentVersion
            <$> workspaceProjects
            
      , Command "properties" ["p"] $
            forEachProject2' properties
            <$> workspaceProjects
            <*> arg "[matching]" maybeGrepPropertyParser

      , Command "snapshots" ["ss"] $
            forEachProject snapshots
            <$> workspaceProjects
            
      , Command "bash" [] $
            forEachProject2 bashAction
            <$> arg "command" text
            <*> workspaceProjects

      , Command "bashProject" [] $
            forEachProject2 bashProjectAction
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
           ++ map (indent 1 . showProject) projects

    showProject p = "{}{}" %% [projectName p, showAliases]
      where
        showAliases = case projectAliases p of
          []  -> ""
          [a] -> " (alias: {})" % a
          as  -> " (aliases: {})" % commas as

comWorkspace :: [Project]
comWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace"
    projects = map (wsProject workspace)
      [ ("cobranca-api", ["cob"])
      , ("comercial", ["com"])
      , ("touch-protocol", ["tproc"])
      , ("integracao-comercial", ["int"])        
      , ("faturamento", ["fat"])
      , ("bpa-comercial", ["bpa"])
      , ("velab-comercial", ["vcom"])
      , ("comercial-lis", ["lis"])
      ]

genWorkspace :: [Project]
genWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace"
    projects = map (wsProject workspace)
      [ ("financeiro", ["fin"])
      , ("cobranca-api", ["cob"])
      , ("touch-protocol", ["tproc"])
      , ("comercial", ["com"])
      , ("integracao-comercial", ["int"])
      , ("faturamento", ["fat"])
      , ("bpa-comercial", ["bpa"])
      , ("autorizacao", ["auto"])
      , ("autorizador", ["aut"])
      , ("velab-comercial", ["vcom"])
      , ("tiss-comercial", ["tiss"])
      , ("atendimento", ["ate"])
      , ("motion-lis-genesis", ["gen"])
      ]

motWorkspace :: [Project]
motWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace"
    projects = map (wsProject workspace)
      [ ("comercial", ["com"])
      , ("velab-root", ["vroot"])                
      , ("velab-api", [])
      , ("velab", [])
      , ("motion-lis", ["mot"])
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

liqWorkspace :: [Project]
liqWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace/"
    projects = map (wsProject workspace)
      [ ("liquibase3", ["liq"])
      -- , ("touch-liquibase-commons", ["liqcom"])
      -- , ("touch-quartz", ["quartz"])
      -- , ("heals-web-admin", ["admin"])
        
      -- , ("touch-property", ["tprop"])
      -- , ("bi-temporal", ["bi"])
      -- , ("beta", ["bta"])
      -- , ("modifiers", ["mod"])
      -- , ("beta-temporal", ["bbt"])

      , ("comercial-lis", ["lis"])
      , ("motion-lis", ["mot"])
      , ("motion-lis-genesis", ["gen"])
      , ("armazenamento", ["armz"])
      ]

wsProject :: FilePath -> (Text, [Text]) -> Project
wsProject ws (name, aliases) = Project name (ws ++ "/" ++ unpack name) aliases

configure :: Action
configure = do
  let bot = "/home/andregr/Code/bot"
  bashInteractive $ "emacs -nw {}/lib/Bot/Config.hs" % bot
  liftIO $ putStrLn "Recompiling"
  output <- bash $ "cd {} && /home/andregr/.cabal/bin/cabal build" % bot
  liftIO $ T.putStrLn output  

changeConfig :: Text -> Action
changeConfig config = do
  unless (config `elem` map configName configurations) $
    throwA $ "Unknown configuration '{}'" % config
  liftIO $ writeTextFile defaultConfigFile  config

maybeGrepPropertyParser :: Parser (Maybe ((Text, Text) -> Bool))
maybeGrepPropertyParser = fmap (fmap grepProperty) $ optional text

grepProperty :: Text -> (Text, Text) -> Bool
grepProperty s (n, v) = or $ map (s `T.isInfixOf`) [n, v]

generateScripts :: [Command a] -> Action
generateScripts commands = do
    void $ forM commands $ \command -> do
      case commandAliases command of
        []    -> return ()
        (name:_) -> liftIO $ do
          T.putStrLn $
            "TODO Created script {} for command {}" %% (name, commandName command)
          writeTextFile (unpack $ "{}/{}" %% (dir, name)) $
            ("#!/bin/bash\n\nbot -{} TODO" % name)
  where
    dir = "/home/andregr/Code/bot/dist/scripts" :: Text
