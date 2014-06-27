{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Bot.Action.Action
  ( bash
  , bashInteractive
  , cd
  , repeatedly
  , repeatedly2
  , silentProjectCommand
  ) where

import Bot.Types
import Bot.Util
import Control.Exception
import Control.Monad
import qualified Data.Text.Lazy as T
import System.Directory
import System.Exit
import System.IO
import System.IO.Temp 
import System.Posix.IO
import System.Process

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

repeatedly :: (Project -> IO ()) -> [Project] -> IO ()
repeatedly action projects = forM_ projects $ \project -> do
    putf "{}:\t\t" (Only $ projectName project) >> action project

repeatedly2 :: (a -> Project -> IO ()) -> a -> [Project] -> IO ()
repeatedly2 action arg1 projects = forM_ projects $ \project -> do
    putf "{}:\t\t" (Only $ projectName project) >> action arg1 project


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
