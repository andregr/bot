{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Bot.Parser 
  ( Command(..)
  , Arg(..)
  , Error(..)
  , Reader
  , Parser
  , (?)
  , allRemaining
  , arg
  , catch
  , commandApplicationParser
  , commandHelp
  , commandParser
  , consumeOne
  , integer
  , get
  , isEndOfInput
  , liftArg
  , liftReader
  , onExcept
  , put
  , runParser
  , runParserFully
  , showReader
  , text
  , throw
  ) where

import Bot.Util ((%), (%%))
import Control.Applicative (Applicative(..), Alternative(..), (<$>), many)
import Control.Applicative.Free (Ap(..), hoistAp, retractAp, liftAp)
import Control.Arrow ((&&&))
import Control.Monad.Loops (whileM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExcept, throwE, catchE)
import Control.Monad.Trans.State (StateT(..), runStateT, liftCatch)
import qualified Control.Monad.Trans.State as S (get, put)
import Data.Monoid (Monoid, (<>))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T (unlines, replicate, unwords, isPrefixOf, tail)
import qualified Data.Text.Lazy.Read as T (decimal)

{-
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
  show (Error es) = show $ T.unlines $ map (\(i, e) -> indent i e) $ zip [1..] es
    where
      indent i e = (T.replicate i  "  ") <> e

catch :: Parser a -> (Error -> Parser a) -> Parser a
catch p h = Parser $ liftCatch catchE (unParser p) (fmap (unParser) h)

throw :: Error -> Parser a
throw = Parser . lift . throwE

onExcept :: Parser a -> Text -> Parser a
onExcept p m = p `catch` (\e -> throw $ Error [m] <> e)

get :: Parser [Text]
get = Parser S.get

put :: [Text] -> Parser ()
put = Parser . S.put

showReader :: Reader a -> Text
showReader (Pure _) = ""
showReader (Ap arg (Pure _)) = argName arg
showReader (Ap arg rest) = argName arg  <> " " <> showReader rest

-- Arg to Reader
arg :: Text -> Parser a -> Reader a
arg name parser = liftAp $ Arg name parser

-- Transform a Reader into a Parser
liftReader :: Reader a -> Parser a
liftReader = retractAp . hoistAp liftArg

liftArg :: Arg a -> Parser a
liftArg (Arg name parser) = parser `onExcept` readFailedMsg name
  where
    readFailedMsg name = "Failed to read argument '{}':" % name

runParser :: Parser a -> [Text] -> Either Error (a, [Text])
runParser p as = runExcept . (flip runStateT as) . unParser $ p

runParserFully :: Parser a -> [Text] -> Either Error a
runParserFully p as = 
  case runParser p as of
    Left e -> Left e
    Right (a, []) -> Right a
    Right (_, unused) -> Left $ Error ["Unused arguments: '{}'" % T.unwords unused]

consumeOne :: Error -> (Text -> Either Error a) -> Parser a
consumeOne noArgError one = get >>= consume
  where
    consume []         = throw noArgError
    consume (arg:args) = either throw (\a -> put args >> return a) (one arg)    

isEndOfInput :: Parser Bool
isEndOfInput = null <$> get

allRemaining :: Parser a -> Parser [a]
allRemaining p = whileM (not <$> isEndOfInput) p

text :: Parser Text
text = consumeOne (Error ["Missing argument"]) Right

integer :: Parser Int
integer = do
  maybeInt <- fmap T.decimal text
  case maybeInt of
    Left _ -> throw $ Error ["Failed to read integer"]
    Right (i, "") -> return i
    Right (_, _) -> throw $ Error ["Failed to read integer"]

(?) :: Parser a -> (a -> Bool) -> Parser a
p ? c = do
  a <- p
  if c a 
    then return a 
    else throw $ Error []
infixl 6 ?

commandHelp :: Command a -> Text
commandHelp c = "-" <> commandName c <> " " <> showReader (applyCommand c)

commandParser :: [Command a] -> Parser (Command a)
commandParser commands = do
    let readCommandName = T.tail <$> text ? ("-" `T.isPrefixOf`)
    name <- readCommandName `onExcept` "Missing command"
    case lookup name commandMap of
      Nothing -> throw $ Error [unknownCommandMsg name]
      Just c  -> return c
  where
    commandMap = map (commandName &&& id) commands
    unknownCommandMsg w = "Unknown command '{}'" % w

commandApplicationParser :: [Command a] -> Parser a
commandApplicationParser commands = do
    command <- commandParser commands
    args <- many $ text ? (not . T.isPrefixOf "-")
    case runParser (liftReader $ applyCommand command) args of
      Right (a, [])      -> return a
      Right (_, unused)  -> throw $ Error [tooManyArgsMsg command unused]
      Left e             -> throw $ (Error [wrongArgMsg command] <> e)
  where
    wrongArgMsg c = "Failed to read arguments for command '{}':" % commandName c
    tooManyArgsMsg c u = "Unused arguments for command '{}': {}" %% 
                            (commandName c, T.unwords u)