{-# LANGUAGE BangPatterns, OverloadedStrings, TypeFamilies #-}

module Bot.Action.Action
  ( throwA
  , bash
  , bashInteractive
  , cd
  , forEachProject
  , forEachProject2
  , forEachProject2'
  , silentProjectCommand
  , bashAction
  , bashProjectAction
  ) where

import Bot.Types
import Bot.Util
import Control.Monad (unless, when, forM_)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.Exit
import System.IO
import System.IO.Temp 
import System.Posix.IO
import System.Process

throwA :: (MonadMask m) => Text -> m a
throwA = throwM . ActionException

bash :: (MonadIO m, MonadReader m, EnvType m ~ Options) =>
        Text -> m Text
bash cmd = do
    copyOutput <- asks optBashCopyOutput
    liftIO $ do
      (readfd, writefd) <- createPipe
      writeh <- fdToHandle writefd

      (_, _, _, p) <- createProcess (shell (unpack cmd)) 
                                             { std_out = UseHandle writeh
                                             , std_err = UseHandle writeh
                                             , close_fds = False
                                             }
      bracket (fdToHandle readfd) hClose $ \h -> do
        {-
        The output needs to be read strictly for two reasons:

        1. Avoid stuck processes due to clogged pipes

          If copyOutput is Off and the caller function doesn't use the
          process output waitForProcess gets stuck due to a clogged pipe:
          the process is waiting for its output to be read before continuing
          but it never is.

        2. Make sure the process is over when this function returns
        -}
        
        strictOutput <- case copyOutput of
          Off      -> T.hGetContents h
          ToFile f -> do
            output <- T.hGetContents h
            appendOutput output f
            return output
          ToStdout -> do
            output <- T.hGetContents h
            hAppendOutput output stdout
            return output
            
        exitCode <- waitForProcess p
          
        case exitCode of
          ExitSuccess   -> return strictOutput
          ExitFailure _ -> throwA strictOutput
  where
    appendOutput :: Text -> FilePath -> IO ()
    appendOutput output path = do
      withFile path AppendMode (hAppendOutput output)

    hAppendOutput :: Text -> Handle -> IO ()
    hAppendOutput output h = do
        -- No buffering for streaming output
        hSetBuffering h NoBuffering
        dir <- getCurrentDirectory
        T.hPutStr h $ 
          "\n\nOutput '{}' in directory '{}':\n--------------\n\n" %% (cmd, pack dir)
        T.hPutStr h output

bashInteractive :: MonadIO m => Text -> m ()
bashInteractive cmd = liftIO $ do
    (_, _, _, p) <- createProcess (shell (unpack cmd)) 
                                     { close_fds = False }
    exitCode <- waitForProcess p
    case exitCode of
      ExitSuccess   -> return ()
      ExitFailure _ -> throwA $ "Interactive command failed"

cd :: (MonadMask m, MonadIO m) => FilePath -> m a -> m a
cd new f = do
  dirExists <- liftIO $ doesDirectoryExist new
  unless dirExists $
    throwA $ "Directory doesn't exist: {}" % pack new
  old <- liftIO $ getCurrentDirectory
  let changeDir p = liftIO $ setCurrentDirectory p
  bracket (changeDir new) (const $ changeDir old) (const f)

showOutput :: MonadIO m => Text -> m ()
showOutput c
    | length (T.lines c) <= 10 = showSimple
    | otherwise                = showPaged
  where
    showSimple = liftIO $ putStrLn (unpack c)
    
    showPaged = liftIO $ do
      putStr "View output? [Y/n] "
      hFlush stdout
      line <- getLine
      when (line `elem` ["", "y", "Y"]) $
        withSystemTempFile "bot" $ \path h -> do
          hPutStrLn h (unpack c)
          hFlush h
          -- +G: start from the end of the output
          bashInteractive ("less +G {}" % pack path)

forEachProject :: (Project -> Action) -> [Project] -> Action
forEachProject action projects = forM_ projects $ \project -> do
    putProjectName project >> action project

forEachProject2 :: (a -> Project -> Action) -> a -> [Project] -> Action
forEachProject2 action arg1 projects = forM_ projects $ \project -> do
    putProjectName project >> action arg1 project

forEachProject2' :: (Project -> a -> Action) -> [Project] -> a -> Action
forEachProject2' action projects arg2 = forM_ projects $ \project -> do
    putProjectName project >> action project arg2

putProjectName :: Project -> ActionM ()
putProjectName p = liftIO $ do
  T.putStr (leftAlign 30 $ projectName p <> ":  ")
  hFlush stdout

silentProjectCommand :: Text -> Project -> Action
silentProjectCommand cmd project =
    cd (projectPath project) $
      (bash cmd >> return ()) `catch` showError
  where 
    showError :: ActionException -> ActionM ()
    showError (ActionException output) = do
      printf "\n\nBash command '{}' failed on project '{}'. Output:" (cmd, projectName project)
      liftIO $ showOutput output
      throwA ""

bashAction :: Text -> Project -> Action
bashAction cmd project = do
  output <- bash $ makeBashCommand (T.replace "{}" (projectName project) cmd)
  liftIO $ do
    T.putStr "\n\n"
    T.putStrLn output      

bashProjectAction :: Text -> Project -> Action
bashProjectAction cmd project =
  cd (projectPath project) $ do
    output <- bash $ makeBashCommand cmd
    liftIO $ do
      T.putStr "\n\n"
      T.putStrLn output

makeBashCommand :: Text -> Text
makeBashCommand cmd = script 
  where   
    -- Several tricks are needed to get bash to expand aliases in
    -- non-intective shells:
    -- * -i option for intective mode (maybe this is enough now?)
    -- * PS1 is set to pretend the shell is interactive, fooling .bashrc
    -- * The expand_aliases option is set
    -- * .bashrc is sourced
    -- * __On a separate line__ the command is executed, because bash only
    --   expands aliases in lines after they are defined
    script = T.unlines [ "/bin/bash -i -c '"
                       , "export PS1=\"-\""
                       , "shopt -s expand_aliases"
                       , "source ~/.bashrc"
                       , cmd
                       , "'"
                       ]

replaceVariables :: [(Text, Text)] -> Text -> Text
replaceVariables vs i = foldl replaceVariable i vs
  where
    replaceVariable i (v,r) = T.replace ("${{}}" % v) r i

interpolate :: [(Text, Text)] -> FilePath -> FilePath -> IO ()
interpolate vs ifn ofn =
  readTextFile ifn >>=
  return . (replaceVariables vs) >>=
  writeTextFile ofn
