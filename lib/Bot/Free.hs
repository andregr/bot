{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot.Free where

import Control.Applicative       ( Applicative(..), Alternative, (<$>), many )
import Control.Applicative.Free  ( Ap(..), hoistAp, retractAp )
import Control.Arrow             ( (&&&) )
import Control.Monad.Loops ( whileM )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Except ( Except, runExcept, throwE, catchE )
import Control.Monad.Trans.State ( StateT(..), runStateT, liftCatch )
import qualified Control.Monad.Trans.State as S ( get, put )
import Data.List (isPrefixOf)

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
    Right (_, unused) -> Left $ "Unused arguments: '" ++ unwords unused ++ "'"

showReader :: Reader a -> String
showReader (Pure _) = ""
showReader (Ap arg (Pure _)) = argName arg
showReader (Ap arg rest) = argName arg  ++ " <*> " ++ showReader rest

liftReader :: Reader a -> Parser a
liftReader = retractAp . hoistAp liftArg

arg :: String -> (String -> Maybe a) -> Parser a
arg name reader = liftArg $ Arg name reader

liftArg :: Arg a -> Parser a
liftArg (Arg name reader) = consumeOne (missingArgMsg name) readArg
  where
    readArg w = maybe (Left $ readFailedMsg name w) Right (reader w)

    readFailedMsg name w = "Failed to read '" ++ name ++ "' from " ++ w
    missingArgMsg name = "Missing argument '" ++ name ++ "'"

debugParser :: Parser [IO a] -> String -> IO ()
debugParser p s = either putStrLn sequence_ $ runParserFully p (words s)

data Command = Command
  { commandName :: String
  , applyCommand :: Parser Action
  }

type Action = IO ()

instance Show Command where
  show = commandName

commandParser :: [Command] -> Parser Command
commandParser commands = do
    name <- tail <$> string ? ("-" `isPrefixOf`)
    case lookup name commandMap of
      Nothing -> throw $ unknownCommandMsg name
      Just c  -> return c
  where
    commandMap = map (commandName &&& id) commands
    unknownCommandMsg w = "Unknown command '" ++ w ++ "'"

actionParser :: [Command] -> Parser Action
actionParser commands = do
    command <- commandParser commands
    args <- many $ string ? (not . isPrefixOf "-")
    let maybeAction = runParser (applyCommand command) args
    case maybeAction of
      Right (action, []) -> return action
      Right (_, unused)  -> throw $ tooManyArgsMsg command unused
      Left e             -> throw $ wrongArgMsg command ++ "\n\t" ++ e
  where
    wrongArgMsg c = "Failed to read arguments for command '" ++ 
                    commandName c ++ "':"
    tooManyArgsMsg c u = "Unused arguments for command '" ++ commandName c ++
                         "': " ++ unwords u

consumeOne :: Error -> (String -> Either Error a) -> Parser a
consumeOne noArgMsg one = get >>= consume
  where
    consume []         = throw noArgMsg
    consume (arg:args) = put args >> either throw return (one arg)    

isEndOfInput :: Parser Bool
isEndOfInput = null <$> get

allRemaining :: Parser a -> Parser [a]
allRemaining p = whileM (not <$> isEndOfInput) p

string :: Parser String
string = consumeOne "Missing argument" Right

(?) :: Parser a -> (a -> Bool) -> Parser a
p ? c = do
  a <- p
  if c a 
    then return a 
    else throw "Condition failed"
infixl 6 ?

greet :: String -> Action
greet s = putStrLn $ "Hello " ++ s ++ "!"

thank :: String -> Action
thank s = putStrLn $ "Thank you, " ++ s ++ "."

repeatPrint :: String -> Int -> Action
repeatPrint m c = sequence_ $ take c $ repeat $ putStrLn m

commands :: [Command]
commands = [ Command "greet" (greet <$> arg "name" Just)
           , Command "thank" (thank <$> arg "name" Just)
           , Command "repeat" (repeatPrint 
                                 <$> arg "message" Just 
                                 <*> arg "count" (Just . read))
           ]