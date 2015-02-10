{-# LANGUAGE OverloadedStrings #-}

module Bot.Parser.Parser
  ( (?)
  , allRemaining
  , arg
  , integer
  , isEndOfInput
  , path
  , runParser
  , runParserFully
  , text
  , constant
  , liftReader
  , catchP
  , throwP
  , onExceptP
  ) where

import Bot.Types
import Bot.Util
import Control.Applicative
import Control.Applicative.Free (hoistAp, retractAp, liftAp)
import Control.Monad.Loops (whileM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExcept, throwE, catchE)
import Control.Monad.Trans.State (StateT(..), runStateT, liftCatch)
import qualified Control.Monad.Trans.State as S (get, put)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Read as T (decimal)

catchP :: Parser a -> (Error -> Parser a) -> Parser a
catchP p h = Parser $ liftCatch catchE (unParser p) (fmap (unParser) h)

throwP :: Error -> Parser a
throwP = Parser . lift . throwE

onExceptP :: Parser a -> Text -> Parser a
onExceptP p m = p `catchP` (\e -> throwP $ Error [m] <> e)

get :: Parser [Text]
get = Parser S.get

put :: [Text] -> Parser ()
put = Parser . S.put

-- Arg to Reader
arg :: Text -> Parser a -> Reader a
arg name parser = liftAp $ Arg name parser

-- Transform a Reader into a Parser
liftReader :: Reader a -> Parser a
liftReader = retractAp . hoistAp liftArg

liftArg :: Arg a -> Parser a
liftArg (Arg name parser) = parser `onExceptP` readFailedMsg name
  where
    readFailedMsg n = "Failed to read argument '{}':" % n

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
    consume []         = throwP noArgError
    consume (arg1:args) = either throwP (\a -> put args >> return a) (one arg1)

isEndOfInput :: Parser Bool
isEndOfInput = null <$> get

allRemaining :: Parser a -> Parser [a]
allRemaining p = whileM (not <$> isEndOfInput) p

text :: Parser Text
text = consumeOne (Error ["Missing argument"]) Right

constant :: Text -> Parser Text
constant c = text ? (== c)

path :: Parser FilePath
path = fmap unpack text

integer :: Parser Int
integer = do
  maybeInt <- fmap T.decimal text
  case maybeInt of
    Left _ -> throwP $ Error ["Failed to read integer"]
    Right (i, "") -> return i
    Right (_, _) -> throwP $ Error ["Failed to read integer"]

(?) :: Parser a -> (a -> Bool) -> Parser a
p ? c = do
  a <- p
  if c a 
    then return a 
    else throwP $ Error []
infixl 6 ?
