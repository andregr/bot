{-# LANGUAGE OverloadedStrings #-}

module Bot.Test.TestParser where

import Bot.Test.TestUtil
import Bot.Parser          
import Control.Applicative ( (<$>), (<*>), pure, (<|>) )

greet :: String -> String
greet s = "Hello " ++ s ++ "!"

thank :: String -> String
thank s = "Thank you, " ++ s ++ "."

repeatString :: Int -> String -> String
repeatString c m = unlines $ take c $ repeat m

commands :: [Command String]
commands = [ Command "greet" (greet <$> arg "name" string)
           , Command "thank" (thank <$> arg "name" string)
           , Command "repeat" (repeatString 
                                    <$> arg "count" digit            
                                    <*> arg "message" (string <|> pure "default"))
           ]

parse :: String -> Either Error [String]
parse s = runParserFully (allRemaining $ commandApplicationParser commands) (words s)

(==>) :: String -> Either Error [String] -> IO ()
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
                                  , "Failed to read digit"
                                  ])

case_unknown_command =
  "-unknown" ==> (Left $ Error ["Unknown command 'unknown'"])

case_missing_command =
  "a" ==> (Left $ Error ["Missing command"])

case_second_command_wrong =
  "-greet a -thank b c" ==> (Left $ Error ["Unused arguments for command 'thank': c"])

tests = $(testGroupGenerator)