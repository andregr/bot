{-# LANGUAGE OverloadedStrings #-}

module Bot.Parser.Command
  ( actionParser
  ) where

import Bot.Parser.Parser
import Bot.Types
import Bot.Util
import Control.Applicative
import Control.Arrow ((&&&))
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as T

commandParser :: [Command a] -> Parser (Command a)
commandParser commands = do
    let readCommandName = T.tail <$> text ? ("-" `T.isPrefixOf`)
    name <- readCommandName `onExceptP` "Missing command"
    case lookup name commandMap of
      Nothing -> throwP $ Error [unknownCommandMsg name]
      Just c  -> return c
  where
    commandMap = map (commandName &&& id) commands
    unknownCommandMsg w = "Unknown command '{}'" % w

applicationParser :: [Command a] -> Parser (Application a)
applicationParser commands = Application <$> parseCommand <*> parseArgs
  where
    parseCommand = commandParser commands
    parseArgs = many $ text ? (not . T.isPrefixOf "-")

actionParser :: [Command a] -> Parser a
actionParser commands = do
    (Application command args) <- applicationParser commands
    case runParser (liftReader $ applyCommand command) args of
      Right (a, [])      -> return a
      Right (_, unused)  -> throwP $ Error [tooManyArgsMsg command unused]
      Left e             -> throwP $ (Error [wrongArgMsg command] <> e)
  where
    wrongArgMsg c = "Failed to read arguments for command '{}':" % commandName c
    tooManyArgsMsg c u = "Unused arguments for command '{}': {}" %% 
                            (commandName c, T.unwords u)
