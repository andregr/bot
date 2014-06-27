{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Bot.Action 
  ( bash
  , bashInteractive
  , cd
  , repeatedly
  , silentlyRepeatedly
  , silentProjectCommand

  -- Maven
  , maven

  -- Git
  , gitBranch
  , gitStatus
  ) where

import Bot.Types (ActionException(..), Project (..))
import Bot.Util (Only(..), printf, (%))
import Control.Applicative ((<$>))
import Control.Exception (catch, throwIO, bracket)
import Control.Monad (when, unless, void, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import System.Directory (doesFileExist, doesDirectoryExist, getCurrentDirectory, setCurrentDirectory)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, hGetContents, hClose, hFlush, stdout)
import System.IO.Temp (withSystemTempFile)
import System.Posix.IO (createPipe, fdToHandle)
import System.Process (createProcess, shell, waitForProcess,
                       CreateProcess(..), StdStream(..))

makeBashCommand :: Text -> Text
makeBashCommand cmd = script 
  where   
    -- Several tricks are needed to get bash to expand aliases in
    -- non-intective shells:
    -- * PS1 is set to pretend the shell is interactive, fooling .bashrc
    -- * The expand_aliases option is set
    -- * .bashrc is sourced
    -- * __On a separate line__ the command is executed, because bash only
    --   expands aliases in lines after they are defined
    script = T.unlines [ "export PS1=\"-\""
                     , "shopt -s expand_aliases"
                     , "source ~/.bash_profile"
                     , cmd
                     ]

bash :: Text -> IO String
bash cmd = do
    (readfd, writefd) <- createPipe
    writeh <- fdToHandle writefd

    (_, _, _, p) <- createProcess (shell (T.unpack $ makeBashCommand cmd)) 
                                           { std_out = UseHandle writeh
                                           , std_err = UseHandle writeh
                                           , close_fds = False
                                           }
    exitCode <- waitForProcess p
    bracket (fdToHandle readfd) hClose $ \h -> do
      !output <- hGetContents h
      case exitCode of
        ExitSuccess   -> return output
        ExitFailure _ -> throwIO $ ActionException $ T.pack output

bashInteractive :: Text -> IO ()
bashInteractive cmd = do
    (_, _, _, p) <- createProcess (shell (T.unpack $ makeBashCommand cmd)) 
                                     { close_fds = False }
    exitCode <- waitForProcess p
    case exitCode of
      ExitSuccess   -> return ()
      ExitFailure _ -> throwIO $ ActionException $ "Interactive command failed"

cd :: FilePath -> IO a -> IO a
cd new f = do
  dirExists <- doesDirectoryExist new
  unless dirExists $
    throwIO $ ActionException $ "Directory doesn't exist: {}" % T.pack new
  old <- getCurrentDirectory
  bracket (setCurrentDirectory new) (const $ setCurrentDirectory old) (const f)

showOutput :: Text -> IO ()
showOutput c
    | length (T.lines c) <= 10 = showSimple
    | otherwise                = showPaged
  where
    showSimple = putStrLn (T.unpack c)
    
    showPaged = do
      putStr "View output? [Y/n] "
      hFlush stdout
      line <- getLine
      when (line `elem` ["", "y", "Y"]) $
        withSystemTempFile "bot" $ \path h -> do
          hPutStrLn h (T.unpack c)
          hFlush h
          -- +G: start from the end of the output
          bashInteractive ("less +G {}" % T.pack path)

maven :: Text -> Project -> IO ()
maven cmd project = do
  let path = projectPath project
  isMavenProject <- liftIO $ doesFileExist $ path ++ "/pom.xml"
  unless isMavenProject $ do
    throwIO $ ActionException $ "Not a project (pom.xml not found): {}" % T.pack path
  silentProjectCommand ("mvn {}" % cmd) project

repeatedly :: (Project -> IO ()) -> [Project] -> IO ()
repeatedly action projects = forM_ projects $ \project -> do
    printf "{}:\t\t" (Only $ projectName project) >> action project

silentlyRepeatedly :: (Project -> IO ()) -> [Project] -> IO ()
silentlyRepeatedly action = repeatedly (\p -> action p >> putStrLn "OK")

silentProjectCommand :: Text -> Project -> IO ()
silentProjectCommand cmd project =
    cd (projectPath project) $
      void (bash cmd) `catch` showError
  where 
    showError :: ActionException -> IO ()
    showError (ActionException output) = do
      printf "Bash command '{}' failed on project '{}'" (cmd, projectName project)
      showOutput output
      throwIO $ ActionException ""

gitBranch :: Project -> Text -> IO ()
gitBranch project branch = silentProjectCommand ("git checkout HEAD -b {}" % branch) project

gitStatus :: Project -> IO ()
gitStatus project = do
    cd (projectPath project) $ do
        changeCount <- getChangeCount
        maybeDivergence <- getDivergence
        branches <- bash "git branch"
        branch <- case parseCurrentBranch branches of
            Just b  -> return b
            Nothing -> throwIO $ ActionException $ "Unexpected branches: {}" % T.pack branches
        printf "branch '{}', {}, {}" ( branch
                                     , formatChangeCount changeCount
                                     , formatDivergence maybeDivergence
                                     )
  where
    getChangeCount :: IO Int
    getChangeCount = countLines <$> bash "git status --porcelain"

    getDivergence :: IO (Maybe (Int, Int))
    getDivergence = do
      do
        a <- countLines <$> bash "git rev-list HEAD@{upstream}..HEAD"
        b <- countLines <$> bash "git rev-list HEAD..HEAD@{upstream}"
        return $ Just (a,b)
      `catch`
        \(e@(ActionException output)) -> do
          if "No upstream configured" `T.isInfixOf` output then return Nothing else throwIO e
    
    countLines = length . lines

    parseCurrentBranch = current . filter ("* " `isPrefixOf`) . lines
        where
            current ['*':' ':b] = Just b
            current _           = Nothing

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
