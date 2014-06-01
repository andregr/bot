{-# LANGUAGE OverloadedStrings #-}

module Bot.Parser 
  ( (?)
  , allRemaining
  , arg
  , commandApplicationParser
  , commandHelp
  , commandParser
  , integer
  , isEndOfInput
  , runParser
  , runParserFully
  , showReader
  , text
  ) where

import Bot.Types (Command(..), Arg(..), Reader, Parser(..), Error(..))
import Bot.Util ((%), (%%))
import Control.Applicative ((<$>), many)
import Control.Applicative.Free (Ap(..), hoistAp, retractAp, liftAp)
import Control.Arrow ((&&&))
import Control.Monad.Loops (whileM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExcept, throwE, catchE)
import Control.Monad.Trans.State (StateT(..), runStateT, liftCatch)
import qualified Control.Monad.Trans.State as S (get, put)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T (unwords, isPrefixOf, tail)
import qualified Data.Text.Lazy.Read as T (decimal)

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