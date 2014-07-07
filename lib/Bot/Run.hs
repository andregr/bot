{-# LANGUAGE OverloadedStrings #-}

module Bot.Run where

import Bot.Config
import Bot.Parser.Command
import Bot.Parser.Parser
import Bot.Types
import Bot.Util
import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Reader
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Exit
import System.IO

run :: [String] -> IO ()
run argStrings = do
    hSetBuffering stdout NoBuffering
  
    when (null args) $ printHelp >> exitFailure
    
    execution <- parseExecution (configCommands configuration) args
    (copyBashOutput, action) <- case execution of
      ShowHelp    -> printHelp >> exitSuccess
      RunAction copyBashOutput a -> return (copyBashOutput, a)
      
    runReaderT action copyBashOutput `catch` \(ActionException e) -> do
      let cmd = T.unwords args
      if T.null e
         then printf "\n\nCommand '{}' failed" (Only cmd)
         else printf "\n\nCommand '{}' failed:\n{}" (cmd, e)
      exitFailure
  where
    args = fmap T.pack argStrings
    printHelp = T.putStrLn (showHelp configuration)

data Execution = ShowHelp | RunAction BashCopyOutput Action

instance Show Execution where
  show ShowHelp = "ShowHelp"
  show (RunAction _ _) = "RunAction"

executionParser :: [Command Action] -> Parser Execution
executionParser commands = do
  let helpParser = fmap Just (constant "-h" <|> constant "--help") <|> pure Nothing
  help <- helpParser
  case help of
    Nothing -> RunAction <$> bashCopyOutputParser <*> actionParser commands
    _       -> return ShowHelp

bashCopyOutputParser :: Parser BashCopyOutput
bashCopyOutputParser = do
  (const ToFile <$> constant "-f" <*> path) <|> pure Off

parseExecution :: [Command Action] -> [Text] -> IO Execution
parseExecution commands args = do
  let maybeAction = runParserFully (executionParser commands) args
  case maybeAction of
    Left e  -> putStrLn (show e) >> exitFailure
    Right a -> return a
