{-# LANGUAGE OverloadedStrings #-}

module Bot.Run where

import Bot.Config
import Bot.Parser.Command
import Bot.Parser.Parser
import Bot.Types
import Bot.Util
import Control.Exception
import Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Exit

run :: [String] -> IO ()
run argStrings = do
    case args of
      []     -> printHelp >> exitFailure
      ["-h"] -> printHelp >> exitSuccess
      _      -> return ()
    action <- parseAction (configCommands configuration) args
    action `catch` \(ActionException e) -> do
      let cmd = T.unwords args
      if T.null e
         then printf "\n\nCommand '{}' failed" (Only cmd)
         else printf "\n\nCommand '{}' failed:\n{}" (cmd, e)
      exitFailure
  where
    args = fmap pack argStrings
    printHelp = T.putStrLn (showHelp configuration)

parseAction :: [Command Action] -> [Text] -> IO Action
parseAction commands args = do
  let maybeAction = runParserFully (actionParser commands) args
  case maybeAction of
    Left e  -> putStrLn (show e) >> exitFailure
    Right a -> return a
