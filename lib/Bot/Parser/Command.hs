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
commandParser available = do
    let readCommandName = T.tail <$> text ? ("-" `T.isPrefixOf`)
    name <- readCommandName `onExceptP` "Expected command (starting with '-')"
    case lookupCommand available name of
      Left n -> throwP $ Error [unknownCommandMsg n]
      Right c  -> return c
  where
    unknownCommandMsg w = "Unknown command '{}'" % w

lookupCommand :: [Command a] -> Text -> Either Text (Command a)
lookupCommand available = lookup' (commandsByName ++ commandsByAliases)
  where
    commandsByName = map (commandName &&& id) available
    commandsByAliases = concatMap (\c -> map (id &&& const c) $ commandAliases c) available
    lookup' m k = case lookup k m of
      Nothing -> Left k
      Just v  -> Right v

applicationParser :: [Command a] -> Parser (Application a)
applicationParser commands = Application <$> parseCommand <*> parseArgs
  where
    parseCommand = commandParser commands
    parseArgs = many $ text ? (not . T.isPrefixOf "-")

actionParser :: [Command a] -> Parser (Text, a)
actionParser commands = do
    (Application command args) <- applicationParser commands
    case runParser (liftReader $ applyCommand command) args of
      Right (a, [])      -> return (T.unwords $ (commandName command : args), a)
      Right (_, unused)  -> throwP $ Error [tooManyArgsMsg command unused]
      Left e             -> throwP $ (Error [wrongArgMsg command] <> e)
  where
    wrongArgMsg c = "Failed to read arguments for command '{}':" % commandName c
    tooManyArgsMsg c u = "Unused arguments for command '{}': {}" %% 
                            (commandName c, T.unwords u)
