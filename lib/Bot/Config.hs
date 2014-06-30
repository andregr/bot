{-# LANGUAGE OverloadedStrings #-}

module Bot.Config where

import Control.Applicative
import Bot.Action.Action
import Bot.Action.Git
import Bot.Action.Maven
import Bot.Util
import Bot.Parser
import Bot.Types
import Data.Monoid
import qualified Data.Text.Lazy as T

configuration :: Configuration
configuration = Configuration commands help
  where
    commands =
      [ Command "clean"
          (repeatedly2 maven <$> pure "clean" <*> workspaceProjects)
          
      , Command "install"
          (repeatedly2 maven <$> pure "install" <*> workspaceProjects)
          
      , Command "status"
          (repeatedly status <$> workspaceProjects)
          
      , Command "createBranch"
          (repeatedly2 createBranch <$> arg "branch" text <*> workspaceProjects)
      ]

    workspace = "/home/andregr/work/workspace"
    
    projects = map wsProject
      [
        "comercial"
      , "faturamento"
      , "bpa-comercial"
      ]

    wsProject name = Project name (T.unpack $ workspace <> "/" <> name)
    
    workspaceProjects = arg "projects" $ projectsParser projects

    help = T.unlines $
                [ "Commands:" ]
             ++ [ "" ]
             ++ map (indent 1 . showHelp) commands
             ++ [ "" ]
             ++ [ "Projects:" ]
             ++ [ "" ]
             ++ map (indent 1 . projectName) projects
