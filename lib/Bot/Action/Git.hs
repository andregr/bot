{-# LANGUAGE OverloadedStrings #-}

module Bot.Action.Git
  ( git
  , createBranch
  , status
  ) where

import Bot.Action.Action
import Bot.Types
import Bot.Util
import Control.Applicative
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

git :: Text -> Project -> Action
git cmd project = silentProjectCommand ("git {}" % cmd) project

createBranch :: Text -> Project -> Action
createBranch branch project = silentProjectCommand ("git checkout HEAD -b {}" % branch) project

status :: Project -> Action
status project = do
    branch <- currentBranch project
    changes <- changeCount project
    maybeDivergence <- divergence project
    liftIO $ T.putStrLn $ commas $ catMaybes [ formatBranch branch
                                             , formatChangeCount changes
                                             , formatDivergence maybeDivergence
                                             ]
  where
    formatBranch b = Just $ T.pack b
    
    formatChangeCount 0 = Nothing
    formatChangeCount 1 = Just "1 file changed"
    formatChangeCount n = Just $ "{} files changed" % (T.pack . show) n

    formatDivergence Nothing = Nothing
    formatDivergence (Just (0,0)) = Nothing
    formatDivergence (Just (ahead,0)) = Just $ "{} ahead" % (T.pack . show) ahead
    formatDivergence (Just (0,behind)) = Just $ "{} behind" % (T.pack . show) behind
    formatDivergence (Just (ahead,behind)) = Just $
      "diverged ({} ahead, {} behind)" %% (show ahead, show behind)

changeCount :: Project -> ActionM Int
changeCount p = cd (projectPath p) $ (length . lines) <$> bash "git status --porcelain"

divergence :: Project -> ActionM (Maybe (Int, Int))
divergence p = cd (projectPath p) $ do
  do
    a <- (length . lines) <$> bash "git rev-list HEAD@{upstream}..HEAD"
    b <- (length . lines) <$> bash "git rev-list HEAD..HEAD@{upstream}"
    return $ Just (a,b)
  `catch`
    \(e@(ActionException output)) -> do
      if "No upstream configured" `T.isInfixOf` output then return Nothing else throwM e

currentBranch :: Project -> ActionM String
currentBranch p = cd (projectPath p) $ do
    branches <- bash "git branch"
    case parseCurrentBranch branches of
      Just b  -> return b
      Nothing -> throwA $ "Unexpected branches: {}" % T.pack branches
  where
    parseCurrentBranch = current . filter ("* " `isPrefixOf`) . lines
      where
        current ['*':' ':b] = Just b
        current _           = Nothing    
