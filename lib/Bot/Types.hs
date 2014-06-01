{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}

module Bot.Types 
  ( Configuration(..)
  , Action
  , Result(..) 
  , Command(..)
  , Arg(..)
  , Reader
  , Parser(..)
  , Error(..)
  ) where

import Control.Applicative (Applicative, Alternative)
import Control.Applicative.Free (Ap)
import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.State (StateT)
import Data.Monoid (Monoid)
import Data.Text.Lazy (Text)

data Configuration = Configuration [Command Action]

type Action = IO Result

data Result = Failure | Success
  deriving (Eq, Show)

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

data Command a = Command
  { commandName :: Text
  , applyCommand :: Reader a
  }

type Reader = Ap Arg

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
  show (Error es) = show $ unlines $ map (\(i, e) -> indent i e) $ zip [1..] es
    where
      indent i e = (concat $ replicate i  "  ") ++ show e  