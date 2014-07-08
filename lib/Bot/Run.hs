{-# LANGUAGE OverloadedStrings #-}

module Bot.Run where

import Bot.Config
import Bot.Parser.Command
import Bot.Parser.Parser
import Bot.Types
import Bot.Util
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Reader
import Data.Monoid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Exit
import System.IO

run :: [String] -> IO ()
run argStrings = do
    hSetBuffering stdout NoBuffering
  
    when (null args) $ printHelp >> exitFailure
    
    execution <- parseExecution args
    (copyBashOutput, _, (cmd, action)) <- case execution of
      ShowHelp                     -> printHelp >> exitSuccess
      RunAction copyBashOutput c a -> return (copyBashOutput, c, a)
      
    runReaderT action copyBashOutput `catch` \(ActionException e) -> do
      if T.null e
         then printf "\n\nCommand '{}' failed" (Only cmd)
         else printf "\n\nCommand '{}' failed:\n{}" (cmd, e)
      exitFailure
  where
    args = fmap T.pack argStrings
    printHelp = T.putStrLn $ T.intercalate "\n" $
                   [ "Usage:" ]
                ++ [ "" ]
                ++ [ indent 1 $ "bot [--help] [--config NAME] " <>
                     "[--show-command-output | --pipe-commands-to FILENAME] COMMAND" ]
                ++ [ "" ]
                ++ [ "where" ]
                ++ [ "" ]
                ++ allConfigsHelp

allConfigsHelp :: Help
allConfigsHelp = concatMap oneConfigHelp configurations
  where
    oneConfigHelp c =    [ "Configuration '{}':" % configName c ]
                      ++ [ "---------------" ]
                      ++ [ "" ]
                      ++ fmap (indent 1) (configHelp c)
                      ++ [ "" ]

data Execution = ShowHelp
               | RunAction BashCopyOutput Configuration (Text, Action)

parseExecution :: [Text] -> IO Execution
parseExecution args = do
  let maybeAction = runParserFully executionParser args
  case maybeAction of
    Left e  -> putStrLn (show e) >> exitFailure
    Right a -> return a

executionParser :: Parser Execution
executionParser = do
  let helpParser = fmap Just (constant "-h" <|> constant "--help") <|> pure Nothing
  help <- helpParser
  case help of
    Just _  -> return ShowHelp
    Nothing -> do
      config <- configurationParser
      let commands = configCommands config
      RunAction <$> bashCopyOutputParser <*> pure config <*> actionParser commands

configurationParser :: Parser Configuration
configurationParser = do
    maybeName <- fmap Just ((,) <$> constant "--config" <*> text) <|> pure Nothing
    case maybeName of
      Just (_, name) -> lookupConfig name
      Nothing        -> return $ defaultConfiguration
  where
    configsByName = map (configName &&& id) configurations
    lookupConfig n = case lookup n configsByName of
      Just c  -> return c
      Nothing -> throwP $ Error [ "Unknown configuration '{}'" % n ]

bashCopyOutputParser :: Parser BashCopyOutput
bashCopyOutputParser = do
      (const ToStdout <$> constant "--show-command-output")
  <|> (const ToFile <$> constant "--pipe-commands-to" <*> path)
  <|> pure Off
