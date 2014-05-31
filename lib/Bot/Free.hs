{-# LANGUAGE DeriveFunctor #-}

module Bot.Free where

import Control.Applicative       ( (<$>), (<*>), pure, Applicative )
import Control.Applicative.Free  ( Ap(..), hoistAp, retractAp )
import Control.Arrow             ( (&&&) )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Except ( Except, runExcept, throwE, catchE )
import Control.Monad.Trans.State ( StateT(..), runStateT, liftCatch )

{-
  "":                          Help
  "-commandN arg1 .. argN":    Ok
  "-commandN arg1 .. arg(<N)": Missing arguments
  "-commandN arg1 .. arg(>N)": Too many arguments
  "-commandN .. badArg ..":    Bad argument
  "-unknown ..":               Unknown command
  "unknown ..":                Missing command 
  "goodApp badApp1 badApp2":   Bad application 1
-}

data Arg a = Arg 
  { argName :: String
  , readArg :: String -> Maybe a
  } deriving Functor

type Reader = Ap Arg

type Error = String
type Errors = [Error]

type Parser = StateT [String] (Except Errors)

showReader :: Reader a -> String
showReader (Pure _) = ""
showReader (Ap arg (Pure _)) = argName arg
showReader (Ap arg rest) = argName arg  ++ " <*> " ++ showReader rest

runReader :: Reader a -> [String] -> Either Errors (a, [String])
runReader r as = runExcept $ runStateT (liftReader r) as

liftReader :: Reader a -> Parser a
liftReader = retractAp . hoistAp liftArg

liftArg :: Arg a -> Parser a
liftArg (Arg name reader) = consumeOne (missingArgMsg name) readArg
  where
    readArg a = maybe (throwE [readFailedMsg name a]) return (reader a)

    readFailedMsg name a = "Failed to read '" ++ name ++ "' from " ++ a
    missingArgMsg name = "Missing argument '" ++ name ++ "'"

runParser :: Parser a -> [String] -> Either Errors a
runParser p as = 
  case runExcept (runStateT p as) of
    Left e -> Left e
    Right (a, []) -> Right a
    Right (_, unused) -> Left $ ["Unused arguments: '" ++ unwords unused ++ "'"]

runParserPrint :: Parser [IO a] -> String -> IO ()
runParserPrint p s = either (putStrLn . unlines) sequence_ $ runParser p (words s)

consumeOne :: Error -> (String -> Except Errors a) -> Parser a
consumeOne noArgMsg one = StateT consume
  where
    consume []     = throwE [noArgMsg]
    consume (a:as) = (,) <$> one a <*> pure as

data Command = Command
  { commandName :: String
  , prepareCommand :: Parser Action
  }

type Action = IO ()

instance Show Command where
  show = commandName

actionParser :: [Command] -> Parser Action
actionParser commands = do
    command <- parseCommand
    liftCatch catchE (prepareCommand command) $ \e -> 
      lift $ throwE (wrongArgMsg command : indent e)
  where
    parseCommand = consumeOne noArgsLeftMsg lookupCommand
    commandMap = map (commandName &&& id) commands
    lookupCommand a = maybe (throwE [unknownCommandMsg a]) return (lookup a commandMap)
    indent as = map ("  " ++) as
    wrongArgMsg command = "Failed to read arguments for command '" ++ commandName command ++ "':"
    unknownCommandMsg a = "Unknown command '" ++ a ++ "'"
    noArgsLeftMsg = "No arguments left"

greet :: String -> Action
greet s = putStrLn $ "Hello " ++ s ++ "!"

thank :: String -> Action
thank s = putStrLn $ "Thank you, " ++ s ++ "."

name :: Parser String
name = liftArg $ Arg "name" Just