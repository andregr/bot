{-# LANGUAGE DeriveFunctor #-}

module Bot.Free where

import Control.Applicative       ( (<$>), (<*>), pure, many, Applicative )
import Control.Applicative.Free  ( Ap(..), hoistAp, retractAp )
import Control.Monad.Trans.State ( StateT(..), runStateT )

data Arg a = Arg 
  { argName :: String
  , readArg :: String -> Maybe a
  } deriving Functor

type Reader = Ap Arg

type Error = String

type Parser = StateT [String]

showReader :: Reader a -> String
showReader (Pure _) = ""
showReader (Ap arg (Pure _)) = argName arg
showReader (Ap arg rest) = argName arg  ++ " <*> " ++ showReader rest

runReader :: Reader a -> [String] -> Either Error (a, [String])
runReader = runParser . liftReader

runParser :: Parser m a -> [String] -> m (a, [String])
runParser = runStateT
    
liftReader :: Reader a -> Parser (Either Error) a
liftReader = retractAp . hoistAp liftArg

liftArg :: Arg a -> Parser (Either Error) a
liftArg (Arg name reader) = parseOne (Left $ "Missing argument " ++ name) readArg
  where
    readArg a = maybe (Left $ "Failed to read " ++ name ++ " from " ++ a) Right (reader a)






parseOne :: Applicative m => m a -> (String -> m a) -> Parser m a
parseOne zero one = StateT $
  \args -> case args of
    []     -> (,) <$> zero <*> pure []
    (a:as) -> (,) <$> one a <*> pure as

parseWord :: (String -> Bool) -> Parser Maybe String
parseWord cond = parseOne Nothing (\a -> if cond a then Just a else Nothing)

parseTask :: [String] -> Parser Maybe (String, [String])
parseTask commands = (,) <$> commandName <*> many commandArg
  where
    isCommandName = (`elem` commands)
    commandName = parseWord isCommandName
    commandArg = parseWord (not . isCommandName)



