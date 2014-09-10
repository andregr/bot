{-# LANGUAGE OverloadedStrings #-}

module Bot.Parser.Project
  ( projectParser
  , projectsParser
  ) where

import Bot.Parser.Parser
import Bot.Types
import Bot.Util
import Control.Applicative
import Control.Arrow
import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Text.Lazy as A
import Data.Either
import Data.List
import qualified Data.Text.Lazy as T

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
        eithers = partitionEithers $ map (lookupProject available) names
    decode (ProjectRange (ms, me)) = case (maybeLookup ms, maybeLookup me) of
        (Right s, Right e) -> Right $ range (s, e) available
        (Right _, Left n) -> Left $ "Unknown project '{}'" % n
        (Left n, Right _) -> Left $ "Unknown project '{}'" % n
        (Left n1, Left n2) -> Left $ "Unknown projects '{}'" % (commas [n1, n2])
      where
        maybeLookup Nothing = Right Nothing
        maybeLookup (Just n) = fmap Just $ (lookupProject available) n
    decode (ProjectBinary bools) = Right $ map snd $ filter fst $ zip bools available

lookupProject :: [Project] -> Text -> Either Text Project
lookupProject available = lookup' (projectsByName ++ projectsByAliases)
  where
    projectsByName = map (projectName &&& id) available
    projectsByAliases = concatMap (\p -> map (id &&& const p) $ projectAliases p) available
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
