{-# LANGUAGE BangPatterns, OverloadedStrings, TypeFamilies #-}

module Bot.Action.Action
  ( throwA
  , bash
  , bashInteractive
  , cd
  , forEachProject
  , forEachProject2
  , silentProjectCommand
  ) where

import Bot.Types
import Bot.Util
import Control.Monad (unless, when, forM_)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Monoid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Directory
import System.Exit
import System.IO
import System.IO.Temp 
import System.Posix.IO
import System.Process

throwA :: (MonadMask m) => Text -> m a
throwA = throwM . ActionException

bash :: (MonadIO m, MonadReader m, EnvType m ~ BashCopyOutput) =>
        Text -> m String
bash cmd = do
    copyOutput <- ask
    liftIO $ do
      (readfd, writefd) <- createPipe
      writeh <- fdToHandle writefd

      (_, _, _, p) <- createProcess (shell (T.unpack cmd)) 
                                             { std_out = UseHandle writeh
                                             , std_err = UseHandle writeh
                                             , close_fds = False
                                             }
      bracket (fdToHandle readfd) hClose $ \h -> do
        output <- hGetContents h
        case copyOutput of
          Off      -> return ()
          ToFile f -> appendOutput f output

        exitCode <- waitForProcess p
          
        -- Avoid lazy IO
        !strictOutput <- return output
          
        case exitCode of
          ExitSuccess   -> return strictOutput
          ExitFailure _ -> throwA $ T.pack strictOutput
  where
    appendOutput :: FilePath -> String -> IO ()
    appendOutput path s = do
      withFile path AppendMode $ \h -> do
        -- No buffering for streaming output
        hSetBuffering h NoBuffering
        hPutStr h s

bashInteractive :: Text -> IO ()
bashInteractive cmd = do
    (_, _, _, p) <- createProcess (shell (T.unpack cmd)) 
                                     { close_fds = False }
    exitCode <- waitForProcess p
    case exitCode of
      ExitSuccess   -> return ()
      ExitFailure _ -> throwA $ "Interactive command failed"

cd :: (MonadMask m, MonadIO m) => FilePath -> m a -> m a
cd new f = do
  dirExists <- liftIO $ doesDirectoryExist new
  unless dirExists $
    throwA $ "Directory doesn't exist: {}" % T.pack new
  old <- liftIO $ getCurrentDirectory
  let changeDir p = liftIO $ setCurrentDirectory p
  bracket (changeDir new) (const $ changeDir old) (const f)

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

forEachProject :: (Project -> Action) -> [Project] -> Action
forEachProject action projects = forM_ projects $ \project -> do
    putProjectName project >> action project

forEachProject2 :: (a -> Project -> Action) -> a -> [Project] -> Action
forEachProject2 action arg1 projects = forM_ projects $ \project -> do
    putProjectName project >> action arg1 project

putProjectName :: Project -> ActionM ()
putProjectName p = liftIO $ do
  T.putStr (leftAlign 20 $ projectName p <> ":  ")
  hFlush stdout

silentProjectCommand :: Text -> Project -> Action
silentProjectCommand cmd project =
    cd (projectPath project) $
      (bash cmd >> return ()) `catch` showError
  where 
    showError :: ActionException -> ActionM ()
    showError (ActionException output) = do
      printf "Bash command '{}' failed on project '{}'" (cmd, projectName project)
      liftIO $ showOutput output
      throwA ""
