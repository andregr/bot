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
import qualified Data.Text.IO as ST -- strict IO to avoid locks between different calls using the same file
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
                 , makeConfiguration "test" testWorkspace
                 , makeConfiguration "tprop" tpropWorkspace
                 ]

makeConfiguration :: Text -> [Project] -> Configuration
makeConfiguration name projects = Configuration name commands help
  where
    gitOk c p = git c p >> (liftIO $ T.putStrLn "ok")
    commands =
      [ Command "configure" ["conf"] $
            pure configure

      , Command "changeConfig" ["cc"] $
            changeConfig
            <$> arg "config" text

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
            forEachProject (\p -> nuke p >> (liftIO $ T.putStrLn "ok"))
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

      , Command "properties" ["p"] $
            forEachProject2' properties
            <$> workspaceProjects
            <*> arg "[matching]" maybeGrepPropertyParser

      , Command "snapshots" ["ss"] $
            forEachProject2' properties
            <$> workspaceProjects
            <*> pure (Just $ grepProperty "SNAPSHOT")
            
      , Command "bash" [] $
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
      [ ("touch-protocol", ["tproc"])
      , ("integracao-comercial", ["int"])
      , ("financeiro", ["fin"])
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
      , ("pagamento", ["pag"])
      , ("produto", ["prod"])
      , ("genesis", [])
      , ("velab-genesis", ["vgen"])
      , ("motion-lis-genesis", ["gen"])
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
maybeGrepPropertyParser = fmap (fmap grepProperty) $ optional text

grepProperty :: Text -> (Text, Text) -> Bool
grepProperty s (n, v) = or $ map (s `T.isInfixOf`) [n, v]
