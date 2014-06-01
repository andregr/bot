{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot.Parser where

import Control.Applicative       ( Applicative(..), Alternative(..), (<$>), many )
import Control.Applicative.Free  ( Ap(..), hoistAp, retractAp )
import Control.Arrow             ( (&&&) )
import Control.Monad.Loops ( whileM )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Except ( Except, runExcept, throwE, catchE )
import Control.Monad.Trans.State ( StateT(..), runStateT, liftCatch )
import qualified Control.Monad.Trans.State as S ( get, put )
import Data.List (isPrefixOf)
import Data.Monoid (Monoid, (<>))

data Arg a = Arg 
  { argName :: String
  , readArg :: Parser a
  } deriving Functor

type Reader = Ap Arg

newtype Error = Error [String]
  deriving (Eq, Monoid)

instance Show Error where
  show (Error []) = "(no message)"
  show (Error es) = unlines $ map (\(i, e) -> indent i e) $ zip [1..] es
    where
      indent i e = (concat $ replicate i  "  ") ++ e

newtype Parser a = Parser { unParser :: StateT [String] (Except Error) a }
  deriving (Functor, Applicative, Alternative, Monad)

catch :: Parser a -> (Error -> Parser a) -> Parser a
catch p h = Parser $ liftCatch catchE (unParser p) (fmap (unParser) h)

throw :: Error -> Parser a
throw = Parser . lift . throwE

get :: Parser [String]
get = Parser S.get

put :: [String] -> Parser ()
put = Parser . S.put

runParser :: Parser a -> [String] -> Either Error (a, [String])
runParser p as = runExcept . (flip runStateT as) . unParser $ p

runParserFully :: Parser a -> [String] -> Either Error a
runParserFully p as = 
  case runParser p as of
    Left e -> Left e
    Right (a, []) -> Right a
    Right (_, unused) -> Left $ Error ["Unused arguments: '" ++ unwords unused ++ "'"]

showReader :: Reader a -> String
showReader (Pure _) = ""
showReader (Ap arg (Pure _)) = argName arg
showReader (Ap arg rest) = argName arg  ++ " <*> " ++ showReader rest

liftReader :: Reader a -> Parser a
liftReader = retractAp . hoistAp liftArg

arg :: String -> Parser a -> Parser a
arg name parser = liftArg $ Arg name parser

liftArg :: Arg a -> Parser a
liftArg (Arg name parser) = parser `catch` (\e -> throw (Error [readFailedMsg name] <> e))
  where
    readFailedMsg name = "Failed to read argument '" ++ name ++ "':"

consumeOne :: Error -> (String -> Either Error a) -> Parser a
consumeOne noArgError one = get >>= consume
  where
    consume []         = throw noArgError
    consume (arg:args) = either throw (\a -> put args >> return a) (one arg)    

isEndOfInput :: Parser Bool
isEndOfInput = null <$> get

allRemaining :: Parser a -> Parser [a]
allRemaining p = whileM (not <$> isEndOfInput) p

string :: Parser String
string = consumeOne (Error ["Missing argument"]) Right

digit :: Parser Int
digit = fmap read $ string ? (`elem` map show ([0..9]:: [Int]))

(?) :: Parser a -> (a -> Bool) -> Parser a
p ? c = do
  a <- p
  if c a 
    then return a 
    else throw $ Error ["Condition failed"]
infixl 6 ?

data Command a = Command
  { commandName :: String
  , applyCommand :: Parser a
  }

type Action = IO ()

instance Show (Command a) where
  show = commandName

commandParser :: [Command a] -> Parser (Command a)
commandParser commands = do
    let readCommandName = tail <$> string ? ("-" `isPrefixOf`)
    name <- readCommandName `catch` (\_ -> throw $ Error ["Missing command"])
    case lookup name commandMap of
      Nothing -> throw $ Error [unknownCommandMsg name]
      Just c  -> return c
  where
    commandMap = map (commandName &&& id) commands
    unknownCommandMsg w = "Unknown command '" ++ w ++ "'"

actionParser :: [Command a] -> Parser a
actionParser commands = do
    command <- commandParser commands
    args <- many $ string ? (not . isPrefixOf "-")
    let maybeAction = runParser (applyCommand command) args
    case maybeAction of
      Right (a, [])      -> return a
      Right (_, unused)  -> throw $ Error [tooManyArgsMsg command unused]
      Left e             -> throw $ (Error [wrongArgMsg command] <> e)
  where
    wrongArgMsg c = "Failed to read arguments for command '" ++ 
                    commandName c ++ "':"
    tooManyArgsMsg c u = "Unused arguments for command '" ++ commandName c ++
                         "': " ++ unwords u