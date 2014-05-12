{-# LANGUAGE OverloadedStrings #-}

module Bot.Parser (
  parser,
  runParser
  ) where

import Bot.Types ( Arg(..), Parser(..), ParserFailure )
import Bot.Util  ( Only(..), (%), commas )
import Control.Applicative ( (<$>), (<*>) )
import Data.Text.Lazy      ( Text )

parser :: Text -> Text -> (Text -> Maybe a) -> Parser a
parser name desc reader = ConsP (fmap const $ Arg name desc reader) (NilP ())

readArg :: Arg a -> Text -> Either Text a
readArg arg t = case argReader arg t of
  Just a -> Right a
  Nothing -> Left $ invalidArgMsg arg t

invalidArgMsg :: Arg a -> Text -> Text
invalidArgMsg arg t = 
  "Couldn't read {} from '{}'. Expected: {}" % (argName arg, t, argDesc arg)

runParser :: Parser a -> [Text] -> Either ParserFailure a
runParser parser ts = case runParserPartial parser ts of
  Right ([], a) -> Right a
  Right (ts, _) -> Left $ "Unused arguments: {}" % (Only $ commas ts)
  Left msg      -> Left msg

runParserPartial :: Parser a -> [Text] -> Either ParserFailure ([Text], a)
runParserPartial (NilP a) ts = Right (ts, a)
runParserPartial (ConsP arg _) [] = 
  Left $ "Missing argument {}: {}" % (argName arg, argDesc arg)
runParserPartial (ConsP arg rest) (t:ts) = 
  fmap <$> readArg arg t <*> runParserPartial rest ts