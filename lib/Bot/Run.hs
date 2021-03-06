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
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.Exit
import System.IO

run :: [String] -> IO ()
run argStrings = do
    hSetBuffering stdout NoBuffering
  
    defaultConfigName <- defaultConfiguration
    let maybeDefaultConfig = lookup defaultConfigName configsByName

    when (null args) $ printHelp maybeDefaultConfig >> exitFailure

    execution <- parseExecution defaultConfigName args
    (options, (cmd, action)) <- case execution of
      ShowHelp              ->
        printHelp maybeDefaultConfig >> exitSuccess
      ShowFullHelp          ->
        printHelp Nothing >> exitSuccess
      RunAction options _ a -> return (options, a)
      
    runAction action options `catch` \(ActionException msg) -> do
      if T.null msg
         then printf "Command '{}' failed" (Only cmd)
         else T.putStrLn msg
      exitFailure
  where
    args = fmap pack argStrings
    printHelp maybeConfig = T.putStrLn $ T.intercalate "\n" $
                   [ "Usage:" ]
                ++ [ "" ]
                ++ [ indent 1
                      $  "bot "
                      <> "[--help] "
                      <> "[--config NAME] "
                      <> "[[--verbose | --show-command-output] | --pipe-commands-to FILENAME] "
                      <> "COMMAND"
                   ]
                ++ [ "" ]
                ++ [ "where" ]
                ++ [ "" ]
                ++ configsHelp maybeConfig

configsHelp :: Maybe Configuration -> Help
configsHelp Nothing = allConfigsHelp
configsHelp (Just config) = oneConfigHelp config

allConfigsHelp :: Help
allConfigsHelp = concatMap oneConfigHelp configurations

oneConfigHelp :: Configuration -> Help
oneConfigHelp c =    [ "Configuration '{}':" % configName c ]
                  ++ [ "---------------" ]
                  ++ [ "" ]
                  ++ fmap (indent 1) (configHelp c)
                  ++ [ "" ]

data Execution = ShowHelp
               | ShowFullHelp
               | RunAction Options Configuration (Text, Action)

parseExecution :: Text -> [Text] -> IO Execution
parseExecution defaultConfig args = do
  let maybeAction = runParserFully (executionParser defaultConfig) args
  case maybeAction of
    Left e  -> print e >> exitFailure
    Right a -> return a

executionParser :: Text -> Parser Execution
executionParser defaultConfig = do
  let helpParser = fmap Just (constant "-h" <|> constant "--help") <|> pure Nothing
  help <- helpParser
  case help of
    Just "-h"  -> return ShowHelp
    Just _ -> return ShowFullHelp
    Nothing -> do
      config <- configurationParser defaultConfig
      let commands = configCommands config
      RunAction <$> optionsParser <*> pure config <*> actionParser commands

configurationParser :: Text -> Parser Configuration
configurationParser defaultConfig = do
    maybeName <- fmap Just ((,) <$> constant "--config" <*> text) <|> pure Nothing
    case maybeName of
      Just (_, name) -> lookupConfig name
      Nothing        -> lookupConfig defaultConfig
  where
    lookupConfig n = case lookup n configsByName of
      Just c  -> return c
      Nothing -> throwP $ Error [ "Unknown configuration '{}'" % n ]

configsByName :: [(Text, Configuration)]
configsByName = map (configName &&& id) configurations

optionsParser :: Parser Options
optionsParser = Options <$> bashCopyOutputParser

bashCopyOutputParser :: Parser BashCopyOutput
bashCopyOutputParser =
      (const ToStdout <$> (constant "--verbose" <|> constant "--show-command-output"))
  <|> (const ToFile <$> constant "--pipe-commands-to" <*> path)
  <|> pure Off
