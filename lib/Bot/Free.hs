{-# LANGUAGE DeriveFunctor #-}

module Bot.Free where

import Control.Applicative       ( (<$>), (<*>), pure, many, Applicative )
import Control.Applicative.Free  ( Ap(..), hoistAp, retractAp )
import Control.Arrow             ( (&&&) )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State ( StateT(..), runStateT, get, put )
import Data.Maybe                ( isNothing )

data Arg a = Arg 
  { argName :: String
  , readArg :: String -> Maybe a
  } deriving Functor

type Reader = Ap Arg

type Error = String

showReader :: Reader a -> String
showReader (Pure _) = ""
showReader (Ap arg (Pure _)) = argName arg
showReader (Ap arg rest) = argName arg  ++ " <*> " ++ showReader rest

runReader :: Reader a -> [String] -> Either Error (a, [String])
runReader = runStateT . liftReader
  where
    liftReader :: Reader a -> StateT [String] (Either Error) a
    liftReader = retractAp . hoistAp liftArg

    liftArg :: Arg a -> StateT [String] (Either Error) a
    liftArg (Arg name reader) = parseOne (Left $ missingArgMsg name) readArg
      where
        readArg a = maybe (Left $ readFailedMsg name a) Right (reader a)

        readFailedMsg name a = "Failed to read " ++ name ++ " from " ++ a
        missingArgMsg name = "Missing argument " ++ name






parseOne :: Applicative m => m a -> (String -> m a) -> StateT [String] m a
parseOne zero one = StateT $
  \args -> case args of
    []     -> (,) <$> zero <*> pure []
    (a:as) -> (,) <$> one a <*> pure as

parseWord :: (String -> Bool) -> StateT [String] Maybe String
parseWord cond = parseOne Nothing (\a -> if cond a then Just a else Nothing)

data Result = Failure | Success

data Command = Command
  { commandName :: String
  , prepareCommand :: StateT [String] (Either Error) Action
  }

data Task = Task 
  { taskCommand :: Command
  , taskArgs :: [String]
  }

type Action = IO Result

instance Show Command where
  show = commandName

instance Show Task where
  show (Task c as) = show c ++ "(" ++ unwords as ++ ")"

taskParser :: [Command] -> StateT [String] Maybe Task
taskParser commands = Task <$> parseCommand <*> many parseArg
  where
    parseCommand = parseOne Nothing lookupCommand
    parseArg = parseOne Nothing (\a -> if isNothing (lookupCommand a) then Just a else Nothing)
    commandMap = map (commandName &&& id) commands
    lookupCommand = flip lookup commandMap

actionParser :: [Command] -> StateT [String] (Either Error) Action
actionParser commands = do
    as <- get
    let maybeTask = runStateT (taskParser commands) as
    (task, unused) <- lift $ maybe (Left $ unknownTasksMsg as) Right maybeTask
    put unused
    lift $ actionParser task
  where
    actionParser :: Task -> Either Error Action
    actionParser (Task c args) = 
      case runStateT (prepareCommand c) args of
        Left e -> Left e
        Right (a, []) -> Right a
        Right (_, unused) -> Left $ unusedArgsMsg unused

    unknownTasksMsg as = "Failed to parse tasks from " ++ show as
    unusedArgsMsg as = "Unused arguments: " ++ show as