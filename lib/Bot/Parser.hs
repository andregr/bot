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
import Data.List
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
  t <- text
  let maybeSpecifier = A.eitherResult $ A.parse projectsSpecifierParser t
  maybeProjects <- case maybeSpecifier of
    Left e -> throwP $ Error [ "Attoparsec failed with error {}" % T.pack e ]
    Right specifier -> return $ decodeProjectsSpecifier available specifier
  case maybeProjects of
    Left e -> throwP $ Error [ e ]
    Right ps -> return ps

data ProjectsSpecifier = ProjectList [Text]
                       | ProjectRange (Maybe Text, Maybe Text)
                       | ProjectBinary [Bool]

decodeProjectsSpecifier :: [Project] -> ProjectsSpecifier -> Either Text [Project]
decodeProjectsSpecifier available = decode 
  where
    decode :: ProjectsSpecifier -> Either Text [Project]
    decode (ProjectList names) = case eithers of
        ([], known)  -> Right known
        (unknown, _) -> Left $ "Unknown projects '{}'" % (commas unknown)
      where
        eithers = partitionEithers $ map (lookup' projectsByName) names
    decode (ProjectRange (ms, me)) = case (maybeLookup ms, maybeLookup me) of
        (Right s, Right e) -> Right $ range (s, e) available
        (Right _, Left n) -> Left $ "Unknown project '{}'" % n
        (Left n, Right _) -> Left $ "Unknown project '{}'" % n
        (Left n1, Left n2) -> Left $ "Unknown projects '{}'" % (commas [n1, n2])
      where
        maybeLookup Nothing = Right Nothing
        maybeLookup (Just n) = fmap Just $ lookup' projectsByName n
    decode (ProjectBinary bools) = Right $ map snd $ filter fst $ zip bools available

    projectsByName = map (projectName &&& id) available
    lookup' m k = case lookup k m of
      Nothing -> Left k
      Just v  -> Right v
    
projectsSpecifierParser :: A.Parser ProjectsSpecifier
projectsSpecifierParser =   (ProjectBinary <$> projectBinaryParser)
                        <|> (ProjectList <$> projectListParser)
                        <|> (ProjectRange <$> projectRangeParser)

projectBinaryParser :: A.Parser [Bool]
projectBinaryParser = fmap fst $ (,)
                          <$> ((fmap . fmap) toBool $ A.many1 $ A.satisfy (`elem` "01"))
                          <*> A.endOfInput
  where
    toBool :: Char -> Bool
    toBool '0' = False
    toBool '1' = True
    toBool _   = error $ "(Assert) Expected 0 or 1"

projectRangeParser :: A.Parser (Maybe Text, Maybe Text)
projectRangeParser = (\s _ e _ -> (s, e))
                           <$> optional projectNameParser
                           <*> A.string ".."
                           <*> optional projectNameParser
                           <*> A.endOfInput

projectListParser :: A.Parser [Text]
projectListParser = fmap fst $ (,)
                        <$> projectNameParser `A.sepBy` (A.string ",")
                        <*> A.endOfInput

projectNameParser :: A.Parser Text
projectNameParser = fmap T.pack $ A.many1 $ A.satisfy (`notElem` ",.")

range :: Eq a => (Maybe a, Maybe a) -> [a] -> [a]
range (ms, me) as = slice (ms >>= index) (me >>= index) as
  where
    index a = elemIndex a as

slice :: Maybe Int -> Maybe Int -> [a] -> [a]
slice ms me as = drop s . take (e + 1) $ as
  where
    s = maybe 0 id ms
    e = maybe (length as - 1) id me
