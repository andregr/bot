{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Bot.Action.Action
  ( bash
  , bashInteractive
  , cd
  , forEachProject
  , forEachProject2
  , silentProjectCommand
  ) where

import Bot.Types
import Bot.Util
import Control.Exception
import Control.Monad
import Data.Monoid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Directory
import System.Exit
import System.IO
import System.IO.Temp 
import System.Posix.IO
import System.Process

bash :: Text -> IO String
bash cmd = do
    (readfd, writefd) <- createPipe
    writeh <- fdToHandle writefd

    (_, _, _, p) <- createProcess (shell (T.unpack cmd)) 
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
    (_, _, _, p) <- createProcess (shell (T.unpack cmd)) 
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

forEachProject :: (Project -> IO ()) -> [Project] -> IO ()
forEachProject action projects = forM_ projects $ \project -> do
    putProjectName project >> action project

forEachProject2 :: (a -> Project -> IO ()) -> a -> [Project] -> IO ()
forEachProject2 action arg1 projects = forM_ projects $ \project -> do
    putProjectName project >> action arg1 project

putProjectName :: Project -> IO ()
putProjectName p = T.putStr (leftAlign 20 $ projectName p <> ":  ")

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
