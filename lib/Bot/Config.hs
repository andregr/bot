{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

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
import Control.Arrow ((***))
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
                 , makeConfiguration "tprop" tpropWorkspace
                 , makeConfiguration "liq" liqWorkspace
                 , makeConfiguration "oco" ocoWorkspace
                 , makeConfiguration "hist" histWorkspace
                 , makeConfiguration "ver" verWorkspace
                 ]

makeConfiguration :: Text -> [Project] -> Configuration
makeConfiguration name projects = Configuration name commands help
  where
    gitOk c p = git c p >> (liftIO $ T.putStrLn $ shellColor Green checkMarkChar)
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
            forEachProject (maven "-DskipTests=true clean install" projectProfiles)
            <$> workspaceProjects
          
      , Command "install" ["i"] $
            forEachProject (maven "-DskipTests=true install" projectProfiles)
            <$> workspaceProjects

      , Command "sources" [] $
            forEachProject (maven "dependency:sources" projectProfiles)
            <$> workspaceProjects
            
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

      , Command "pom" [] $
            forEachProject pom
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

-- releaser
release_test_root = ("release-test-root", ["root"])
release_test_alpha = ("release-test-alpha", ["alpha"])
release_test_beta = ("release-test-beta", ["beta"])
release_test_charlie = ("release-test-charlie", ["charlie"])

-- tprop
touch_property = ("touch-property", ["tprop"])
bi_temporal = ("bi-temporal", ["bi"])
beta = ("beta", ["bta"])
classificacao = ("classificacao", ["class"])
modifiers = ("modifiers", ["mod"])
beta_temporal = ("beta-temporal", ["bbt"])

-- app-history
app_history = ("app-history", ["hist"])

-- comercial
touch_protocol = ("touch-protocol", ["tproc"])
integracao_comercial = ("integracao-comercial", ["int"])        
financeiro = ("financeiro", ["fin"])
cobranca_api = ("cobranca-api", ["cob"])
comercial = ("comercial", ["com"])
faturamento = ("faturamento", ["fat"])
bpa_comercial = ("bpa-comercial", ["bpa"])
autorizacao = ("autorizacao", ["auto"])
autorizador = ("autorizador", ["aut"])
velab_comercial = ("velab-comercial", ["vcom"])
tiss_comercial = ("tiss-comercial", ["tiss"])
comercial_lis = ("comercial-lis", ["lis"])

-- motion-lis
velab_root = ("velab-root", ["vroot"])                
velab_api = ("velab-api", [])
velab = ("velab", [])
motion_lis = ("motion-lis", ["motlis"])

-- motion-lis-genesis
atendimento = ("atendimento",["ate"])
genesis = ("genesis",[])
velab_genesis = ("velab-genesis",["vgen"])
motion_lis_genesis = ("motion-lis-genesis", ["gen"])

-- veris-pack
pessoas = ("pessoas", ["pes"])
pessoas_comercial = ("pessoas-comercial", ["pescom"])
veris_pack = ("veris-pack", ["ver"])

-- outros
ocorrencias = ("ocorrencias", ["oco"])
armazenamento = ("armazenamento", ["armz"])
liquibase3 = ("liquibase3", ["liq"])
touch_liquibase_commons = ("touch-liquibase-commons", ["liqcom"])
touch_quartz = ("touch-quartz", ["quartz"])
heals_web_admin = ("heals-web-admin", ["admin"])

projectProfiles :: [(Text, [Text])]
projectProfiles = map (fst *** id) $
  [(veris_pack, ["desenvolvimento"])]

comWorkspace :: [Project]
comWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace"
    projects = map (wsProject workspace)
      [ cobranca_api, comercial, touch_protocol, integracao_comercial,
        faturamento, bpa_comercial, velab_comercial, comercial_lis
      ]

genWorkspace :: [Project]
genWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace"
    projects = map (wsProject workspace)
      [ financeiro, cobranca_api, touch_protocol, integracao_comercial, comercial,
        faturamento, bpa_comercial, autorizacao, autorizador, velab_comercial,
        tiss_comercial, genesis, velab_genesis, atendimento, motion_lis_genesis
      ]

motWorkspace :: [Project]
motWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace"
    projects = map (wsProject workspace)
      [ comercial, velab_root, velab_api, velab, motion_lis_genesis ]

releaserWorkspace :: [Project]
releaserWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace/releaser/test/releaser_workspace"
    projects = map (wsProject workspace)
      [ release_test_root, release_test_alpha, release_test_beta, release_test_charlie ]

tpropWorkspace :: [Project]
tpropWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace/"
    projects = map (wsProject workspace)
      [ touch_property, bi_temporal, beta, classificacao, modifiers, beta_temporal ]

liqWorkspace :: [Project]
liqWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace/"
    projects = map (wsProject workspace)
      [ liquibase3, touch_liquibase_commons, touch_quartz, heals_web_admin,
        touch_property, bi_temporal, beta, modifiers, beta_temporal, comercial_lis,
        motion_lis, motion_lis_genesis, armazenamento ]

ocoWorkspace :: [Project]
ocoWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace"
    projects = map (wsProject workspace)
      [ faturamento, ocorrencias, autorizacao, autorizador, atendimento, motion_lis_genesis ]

histWorkspace :: [Project]
histWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace"
    projects = map (wsProject workspace)
      [ app_history, comercial_lis, veris_pack ]

verWorkspace :: [Project]
verWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace"
    projects = map (wsProject workspace)
      [ financeiro, cobranca_api, touch_protocol, integracao_comercial, comercial,
        faturamento, bpa_comercial, autorizacao, autorizador, velab_comercial,
        tiss_comercial, genesis, pessoas, pessoas_comercial, veris_pack ]

wsProject :: FilePath -> (Text, [Text]) -> Project
wsProject ws (name, aliases) = Project name (ws ++ "/" ++ unpack name) aliases

configure :: Action
configure = do
  let bot = "/home/andregr/Code/bot" :: Text
  bashInteractive $ "emacs -nw {}/lib/Bot/Config.hs" % bot
  liftIO $ putStrLn "Recompiling"
  output <- bash $ "cd {} && /home/andregr/.cabal/bin/cabal build" % bot
  liftIO $ T.putStrLn output  

pom :: Project -> Action
pom project = do
  bashInteractive $ "emacs -nw {}/pom.xml" % projectPath project

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
