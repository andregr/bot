{-# LANGUAGE OverloadedStrings #-}

module Bot.Action.Git
  ( git
  , createBranch
  , fetch
  , status
  , nuke
  , deleteBranches
  ) where

import Bot.Action.Action
import Bot.Types
import Bot.Util
import Control.Applicative
import Control.Monad (void)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Maybe
import Data.Monoid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

git :: Text -> Project -> Action
git cmd project = silentProjectCommand ("git {}" % cmd) project

createBranch :: Text -> Project -> Action
createBranch branch project = silentProjectCommand ("git checkout HEAD -b {}" % branch) project

fetch :: Project -> Action
fetch project = do
  git "fetch" project
  status project

status :: Project -> Action
status project = do
    void $ cd (projectPath project) $ bash "git add -A ."
    branch <- currentBranch project
    changes <- changeCount project
    maybeDivergence <- divergence project
    liftIO $ T.putStrLn $ commas $ catMaybes [ formatBranch branch
                                             , formatChangeCount changes
                                             , formatDivergence maybeDivergence
                                             ]
  where
    formatBranch b = Just b
    
    formatChangeCount 0 = Nothing
    formatChangeCount 1 = Just "1 file changed"
    formatChangeCount n = Just $ "{} files changed" % (T.pack . show) n

    formatDivergence Nothing = Nothing
    formatDivergence (Just (0,0)) = Nothing
    formatDivergence (Just (ahead,0)) = Just $ "{} ahead" % (T.pack . show) ahead
    formatDivergence (Just (0,behind)) = Just $ "{} behind" % (T.pack . show) behind
    formatDivergence (Just (ahead,behind)) = Just $
      "diverged ({} ahead, {} behind)" %% (show ahead, show behind)

deleteBranches :: Project -> Action
deleteBranches project = cd (projectPath project) $ do
    void $ bash "git branch | sed '/*/d' | xargs git branch -D"
  `catch`
    \(e@(ActionException output)) -> do
      -- Ignore error if only branch is master
      if "branch name required" `T.isInfixOf` output
        then return ()
        else throwM e

nuke :: Project -> Action
nuke project = cd (projectPath project) $ do
  void $ bash $ "git add -A . && git stash save && git fetch" <>
                " && git checkout master; git reset --hard origin/master"
  deleteBranches project

changeCount :: Project -> ActionM Int
changeCount p = cd (projectPath p) $ (length . T.lines) <$> bash "git status --porcelain"

divergence :: Project -> ActionM (Maybe (Int, Int))
divergence p = cd (projectPath p) $ do
  do
    a <- (length . T.lines) <$> bash "git rev-list HEAD@{upstream}..HEAD"
    b <- (length . T.lines) <$> bash "git rev-list HEAD..HEAD@{upstream}"
    return $ Just (a,b)
  `catch`
    \(e@(ActionException output)) -> do
      if "No upstream configured" `T.isInfixOf` output then return Nothing else throwM e

currentBranch :: Project -> ActionM Text
currentBranch p = cd (projectPath p) $ do
    branches <- bash "git branch"
    case parseCurrentBranch branches of
      Just b  -> return b
      Nothing -> throwA $ "Unexpected branches: {}" % branches
  where
    parseCurrentBranch = current . filter ("* " `T.isPrefixOf`) . T.lines
      where
        current [s] = Just $ T.replace "* " "" s
        current _   = Nothing    
