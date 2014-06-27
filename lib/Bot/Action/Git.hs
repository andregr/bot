{-# LANGUAGE OverloadedStrings #-}

module Bot.Action.Git
  ( createBranch
  , status
  ) where

import Bot.Action.Action
import Bot.Types
import Bot.Util
import Control.Applicative
import Control.Exception
import Data.List
import qualified Data.Text.Lazy as T

createBranch :: Text -> Project -> IO ()
createBranch branch project = silentProjectCommand ("git checkout HEAD -b {}" % branch) project

status :: Project -> IO ()
status project = do
    branch <- currentBranch project
    changes <- changeCount project
    maybeDivergence <- divergence project
    printf "branch '{}', {}, {}" ( branch
                                 , formatChangeCount changes
                                 , formatDivergence maybeDivergence
                                 )
  where
    formatChangeCount 0 = "clean"
    formatChangeCount 1 = "1  change"
    formatChangeCount n = show n ++ " changes"

    formatDivergence Nothing = "not tracking"
    formatDivergence (Just (0,0)) = "up-to-date"
    formatDivergence (Just (ahead,0)) = show ahead ++ " ahead"
    formatDivergence (Just (0,behind)) = show behind ++ " behind"
    formatDivergence (Just (ahead,behind)) = "diverged (" ++ 
                                             show ahead ++ " ahead, " ++
                                             show behind ++ " behind" ++
                                             ")"

changeCount :: Project -> IO Int
changeCount p = cd (projectPath p) $ (length . lines) <$> bash "git status --porcelain"

divergence :: Project -> IO (Maybe (Int, Int))
divergence p = cd (projectPath p) $ do
  do
    a <- (length . lines) <$> bash "git rev-list HEAD@{upstream}..HEAD"
    b <- (length . lines) <$> bash "git rev-list HEAD..HEAD@{upstream}"
    return $ Just (a,b)
  `catch`
    \(e@(ActionException output)) -> do
      if "No upstream configured" `T.isInfixOf` output then return Nothing else throwIO e

currentBranch :: Project -> IO String
currentBranch p = cd (projectPath p) $ do
    branches <- bash "git branch"
    case parseCurrentBranch branches of
      Just b  -> return b
      Nothing -> throwIO $ ActionException $ "Unexpected branches: {}" % T.pack branches
  where
    parseCurrentBranch = current . filter ("* " `isPrefixOf`) . lines
      where
        current ['*':' ':b] = Just b
        current _           = Nothing    
