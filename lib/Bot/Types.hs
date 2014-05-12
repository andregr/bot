{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Types (
  Command(..),
  Action(..),
  Parser(..),
  Arg(..),
  ParserFailure
  ) where

import Control.Applicative ( Applicative(..), (<$>) )
import Data.Text.Lazy      ( Text )

data Command = Command
  { commName :: Text
  , commDesc :: Text
  , commParser :: Parser Action
  }

data Action = Action

data Parser a where
  NilP :: a -> Parser a
  ConsP :: Arg (x -> a) -> Parser x -> Parser a

instance Functor Parser where
  fmap f (NilP a) = NilP (f a)
  fmap f (ConsP argxa px) = ConsP (fmap (f .) argxa) px

instance Applicative Parser where
  pure = NilP
  (NilP f) <*> p = fmap f p
  (ConsP arg rest) <*> p = ConsP (fmap uncurry arg) ((,) <$> rest <*> p)

data Arg a = Arg
  { argName :: Text
  , argDesc :: Text
  , argReader :: Text -> Maybe a 
  }

instance Functor Arg where
  fmap f arg = arg { argReader = ((fmap . fmap) f (argReader arg)) }

type ParserFailure = Text

actionParser :: Command -> Parser Action
actionParser (Command name _ parser) 
    = ConsP (Arg "" "" nameMatcher) (fmap id parser)
  where
    nameMatcher :: Text -> Maybe (Action -> Action)
    nameMatcher t
      | name == t = Just id
      | otherwise = Nothing