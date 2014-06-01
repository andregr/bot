{-# LANGUAGE OverloadedStrings #-}

module Bot.Test.TestParser where

import Bot.Test.TestUtil
import Bot.Parser
import Bot.Types
import Control.Applicative ( (<$>), (<*>), pure, (<|>) )
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T (unlines, words)

greet :: Text -> Text
greet s = "Hello " <> s <> "!"

thank :: Text -> Text
thank s = "Thank you, " <> s <> "."

repeatText :: Int -> Text -> Text
repeatText c m = T.unlines $ take c $ repeat m

commands :: [Command Text]
commands = [ Command "greet" (greet <$> arg "name" text)
           , Command "thank" (thank <$> arg "name" text)
           , Command "repeat" (repeatText 
                                    <$> arg "count" integer            
                                    <*> arg "message" (text <|> pure "default"))
           ]

parse :: Text -> Either Error [Text]
parse s = runParserFully (allRemaining $ commandApplicationParser commands) (T.words s)

(==>) :: Text -> Either Error [Text] -> IO ()
s ==> r = parse s @?= r
infixl ==>

case_command_help =
  map commandHelp commands @?= [ "-greet name"
                               , "-thank name"
                               , "-repeat count message"]

case_empty = 
  "" ==> Right []

case_ok = 
  "-greet Andre -repeat 2 Hi!" ==> Right [ "Hello Andre!"
                                         , "Hi!\nHi!\n"
                                         ]

case_default_argument =
  "-repeat 2" ==> Right ["default\ndefault\n"]

case_missing_args = 
  "-greet" ==> (Left $ Error [ "Failed to read arguments for command 'greet':"
                             , "Failed to read argument 'name':"
                             , "Missing argument"
                             ])

case_too_many_args = 
  "-greet a b c" ==> (Left $ Error ["Unused arguments for command 'greet': b c"])

case_bad_argument = 
  "-repeat x a" ==> (Left $ Error [ "Failed to read arguments for command 'repeat':"
                                  , "Failed to read argument 'count':"
                                  , "Failed to read integer"
                                  ])

case_unknown_command =
  "-unknown" ==> (Left $ Error ["Unknown command 'unknown'"])

case_missing_command =
  "a" ==> (Left $ Error ["Missing command"])

case_second_command_wrong =
  "-greet a -thank b c" ==> (Left $ Error ["Unused arguments for command 'thank': c"])

tests = $(testGroupGenerator)