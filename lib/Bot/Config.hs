{-# LANGUAGE OverloadedStrings #-}

module Bot.Config where

import Bot.Action.Action
import Bot.Action.Git
import Bot.Action.Maven
import Bot.Parser.Parser
import Bot.Parser.Project
import Bot.Types
import Bot.Util
import Control.Applicative
import Data.Monoid
import qualified Data.Text.Lazy as T

defaultConfiguration :: Configuration
defaultConfiguration = configurations !! 0

configurations :: [Configuration]
configurations = [ makeConfiguration "real" realWorkspace
                 , makeConfiguration "test" testWorkspace
                 ]

makeConfiguration :: Text -> [Project] -> Configuration
makeConfiguration name projects = Configuration name commands help
  where
    commands =
      [ Command "clean" $
          forEachProject2 maven <$> pure "clean" <*> workspaceProjects
          
      , Command "install" $
          forEachProject2 maven <$> pure "install" <*> workspaceProjects
          
      , Command "status" $
          forEachProject status <$> workspaceProjects
          
      , Command "createBranch" $
          forEachProject2 createBranch <$> arg "branch" text <*> workspaceProjects
          
      , Command "version" $
          forEachProject version <$> workspaceProjects

      , Command "properties" $
          forEachProject properties <$> workspaceProjects
      ]
    
    workspaceProjects = arg "projects" $ projectsParser projects

    help = T.unlines $
                [ "Commands:" ]
             ++ [ "" ]
             ++ map (indent 1 . showHelp) commands
             ++ [ "" ]
             ++ [ "Projects:" ]
             ++ [ "" ]
             ++ map (indent 1 . projectName) projects

realWorkspace :: [Project]
realWorkspace = projects
  where
    workspace = "/home/andregr/work/workspace"
    projects = map wsProject
      [
        "financeiro"
      , "cobranca-api"
      , "geradorrps"
      , "comercial"
      , "faturamento"
      , "bpa-comercial"
      ]
    wsProject name = Project name (T.unpack $ workspace <> "/" <> name)          


testWorkspace :: [Project]
testWorkspace = projects
  where
    workspace = "/Users/andre/Code/bot/test/data"
    projects = map wsProject
      [
        "my-app"
      , "my-app2"
      , "my-app3"
      , "my-app4"
      , "my-app5"
      ]
    wsProject name = Project name (T.unpack $ workspace <> "/" <> name)    
