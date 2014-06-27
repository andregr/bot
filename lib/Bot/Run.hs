{-# LANGUAGE OverloadedStrings #-}

module Bot.Run where

import Bot.Action.Action
import Bot.Action.Maven
import Bot.Action.Git
import Bot.Parser
import Bot.Types
import Bot.Util
import Control.Applicative
import Control.Exception
import Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Exit

project :: FilePath -> Parser Project
project workspace = do
  t <- text
  let maybeProjects = runParserFully (projects workspace) [t]
  case maybeProjects of
    Left e    -> throwP e
    Right [p] -> return p
    Right _   -> throwP $ Error ["Failed to read project from '{}'" % t]

projects :: FilePath -> Parser [Project]
projects workspace = do
  parts <- T.splitOn "," <$> text
  return $ fmap (\n -> Project n (workspace ++ "/" ++ T.unpack n)) parts

configuration :: Configuration
configuration = Configuration
    [ Command "clean"
          (repeatedly2 maven <$> pure "clean" <*> workspaceProjects)
    , Command "install"
          (repeatedly2 maven <$> pure "install" <*> workspaceProjects)
    , Command "status"
          (repeatedly status <$> workspaceProjects)
    , Command "createBranch"
          (repeatedly2 createBranch <$> arg "branch" text <*> workspaceProjects)
    ]
  where
    ws = "/home/andregr/work/workspace"
    workspaceProjects = arg "projects" $ projects ws

run :: [String] -> IO ()
run argStrings = do
    case args of
      ["-h"] -> T.putStrLn (help configuration) >> exitSuccess
      _      -> return ()
    action <- parseAction (configCommands configuration) args
    action `catch` \(ActionException e) -> do
      let cmd = T.unwords args
      if T.null e
         then printf "Command '{}' failed" (Only cmd)
         else printf "Command '{}' failed:\n{}" (cmd, e)
      exitFailure
  where
    args = fmap pack argStrings

parseAction :: [Command Action] -> [Text] -> IO Action
parseAction commands args = do
  let maybeAction = runParserFully (commandApplicationParser commands) args
  case maybeAction of
    Left e  -> putStrLn (show e) >> exitFailure
    Right a -> return a

help :: Configuration -> Text
help config =T.unlines $
  [ "Commands:\n" ] ++
  fmap commandHelp (configCommands config)
