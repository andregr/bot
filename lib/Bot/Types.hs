{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Bot.Types 
  ( Action
  , ActionM
  , Options(..)
  , BashCopyOutput(..)
  , ActionException(..)
  , Project(..)
  , Application(..)
  , Configuration(..)
  , Command(..)
  , Arg(..)
  , Reader
  , Parser(..)
  , Error(..)
  , ShowHelp(..)
  , Help
  ) where

import Bot.Util
import Control.Applicative (Applicative, Alternative)
import Control.Applicative.Free
import Control.Exception (Exception)
import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.State
import qualified Control.Monad.Trans.Reader as Trans
import Data.Monoid
import qualified Data.Text.Lazy as T
import Data.Typeable (Typeable)

type Action = Trans.ReaderT Options IO ()

type ActionM = Trans.ReaderT Options IO

data Options = Options
  { optBashCopyOutput :: BashCopyOutput
  }

data BashCopyOutput = Off | ToStdout | ToFile FilePath

data ActionException = ActionException Text
  deriving (Show, Typeable)

instance Exception ActionException

data Project = Project { projectName :: Text, projectPath :: FilePath }
  deriving Show

instance Eq Project where
  a == b = projectName a == projectName b

data Application a = Application
  { applicationCommand :: Command a
  , applicationArgs :: [Text]
  }

data Configuration = Configuration
  { configName :: Text
  , configCommands :: [Command Action]
  , configHelp :: Help
  }

{-
Parsing:

A Command has a Reader, which is a function applied to a heterogeneous
sequence of argument parsers. A Reader is implemented as a free Applicative so 
that the argument parsers can be analysed for help text extraction while still 
maintaining type safety in the function application. A Reader can be run by
transformation into a Parser.

A Parser is formed by a StateT and Except Monad stack and is more flexible than a 
Reader, which is just an applicative. This restriction is intended to ease the static
analysis of Readers for the extraction of help text. For example, a monadic Reader 
could change the expected arguments based on previous results. Even an Alternative 
Reader would make help generation considerably more difficult due to the Left 
Distribution law, which causes free instances to distribute <|>, breaking 
alternatives into groups which would need to be reassociated.

The restriction on Readers is only intended for the combination of Args and
not for Args themselves, which contain Parsers. This means that optional arguments, 
for example, can be implemented using the Alternative functions on Parsers.
-}

type Help = [Text]

class ShowHelp a where
  showHelp :: a -> [Text]

data Command a = Command
  { commandName :: Text
  , applyCommand :: Reader a
  }

instance ShowHelp (Command a) where
  showHelp c = ["-" <> commandName c <> " " <> showReader (applyCommand c)]

type Reader = Ap Arg

showReader :: Reader a -> Text
showReader (Pure _) = ""
showReader (Ap arg (Pure _)) = argName arg
showReader (Ap arg rest) = argName arg  <> " " <> showReader rest

data Arg a = Arg 
  { argName :: Text
  , readArg :: Parser a
  } deriving Functor

-- Stack of errors of increasing specificity
newtype Error = Error [Text]
  deriving (Eq, Monoid)

newtype Parser a = Parser { unParser :: StateT [Text] (Except Error) a }
  deriving (Functor, Applicative, Alternative, Monad)

instance Show Error where
  show (Error []) = "(no message)"
  show (Error es) = unlines $ map (\(i, e) -> T.unpack $ indent i e) $ zip [0..] es
