{-# LANGUAGE OverloadedStrings #-}

module Bot.Parser 
  ( (?)
  , allRemaining
  , arg
  , actionParser
  , applicationParser
  , commandParser
  , integer
  , isEndOfInput
  , path
  , runParser
  , runParserFully
  , text
  , throwP
  , projectParser
  , projectsParser
  ) where

import Bot.Types
import Bot.Util
import Control.Arrow ((&&&))
import Control.Applicative
import Control.Applicative.Free (hoistAp, retractAp, liftAp)
import Control.Monad.Loops (whileM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExcept, throwE, catchE)
import Control.Monad.Trans.State (StateT(..), runStateT, liftCatch)
import qualified Control.Monad.Trans.State as S (get, put)
import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Text.Lazy as A
import Data.Either
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T (decimal)

catch :: Parser a -> (Error -> Parser a) -> Parser a
catch p h = Parser $ liftCatch catchE (unParser p) (fmap (unParser) h)

throwP :: Error -> Parser a
throwP = Parser . lift . throwE

onExcept :: Parser a -> Text -> Parser a
onExcept p m = p `catch` (\e -> throwP $ Error [m] <> e)

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
    consume []         = throwP noArgError
    consume (arg:args) = either throwP (\a -> put args >> return a) (one arg)    

isEndOfInput :: Parser Bool
isEndOfInput = null <$> get

allRemaining :: Parser a -> Parser [a]
allRemaining p = whileM (not <$> isEndOfInput) p

text :: Parser Text
text = consumeOne (Error ["Missing argument"]) Right

path :: Parser FilePath
path = fmap T.unpack text

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

commandParser :: [Command a] -> Parser (Command a)
commandParser commands = do
    let readCommandName = T.tail <$> text ? ("-" `T.isPrefixOf`)
    name <- readCommandName `onExcept` "Missing command"
    case lookup name commandMap of
      Nothing -> throwP $ Error [unknownCommandMsg name]
      Just c  -> return c
  where
    commandMap = map (commandName &&& id) commands
    unknownCommandMsg w = "Unknown command '{}'" % w

applicationParser :: [Command a] -> Parser (Application a)
applicationParser commands = Application <$> parseCommand <*> parseArgs
  where
    parseCommand = commandParser commands
    parseArgs = many $ text ? (not . T.isPrefixOf "-")

actionParser :: [Command a] -> Parser a
actionParser commands = do
    (Application command args) <- applicationParser commands
    case runParser (liftReader $ applyCommand command) args of
      Right (a, [])      -> return a
      Right (_, unused)  -> throwP $ Error [tooManyArgsMsg command unused]
      Left e             -> throwP $ (Error [wrongArgMsg command] <> e)
  where
    wrongArgMsg c = "Failed to read arguments for command '{}':" % commandName c
    tooManyArgsMsg c u = "Unused arguments for command '{}': {}" %% 
                            (commandName c, T.unwords u)

projectParser :: [Project] -> Parser Project
projectParser available = do
  w <- text
  let maybeProjects = runParserFully (projectsParser available) [w]
  case maybeProjects of
    Left e    -> throwP e
    Right [p] -> return p
    Right _   -> throwP $ Error ["Failed to read project from '{}'" % w]


-- Possibilities:
-- p1,p2,p3
-- p1..p3
-- p1..
-- ..p3
-- ..
-- 111
projectsParser :: [Project] -> Parser [Project]
projectsParser available = do
    names <- T.splitOn "," <$> text
    let eithers = partitionEithers $ map (lookup' projectsByName) names
    case eithers of
      ([], known)  -> return known
      (unknown, _) -> throwP $ Error [ "Unknown projects: {}" % (commas unknown) ]
  where
    projectsByName = map (projectName &&& id) available
    lookup' m k = case lookup k m of
      Nothing -> Left k
      Just v  -> Right v

projectRangeParser :: [Project] -> A.Parser (Maybe Project, Maybe Project)
projectRangeParser ps = (\s _ e _ -> (s, e))
                           <$> optional (projectNameParser ps)
                           <*> A.string ".."
                           <*> optional (projectNameParser ps)
                           <*> A.endOfInput

projectListParser :: [Project] -> A.Parser [Project]
projectListParser ps = (projectNameParser ps) `A.sepBy` (A.string ",")

projectNameParser :: [Project] -> A.Parser Project
projectNameParser available = do
    name <- T.pack <$> nameParser
    case lookup name projectsByName of
      Nothing -> fail $ T.unpack $ "Unknown project: {}" % name
      Just p  -> return p
  where
    nameParser = many $ A.satisfy (`notElem` ",.")
    projectsByName = map (projectName &&& id) available

