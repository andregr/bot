{-# LANGUAGE DeriveFunctor #-}

module Bot.Free where

import Control.Alternative.Free
import Control.Applicative
import Data.List

data Fun a = Fun 
  { funName :: String
  , funValue :: a
  } deriving Functor

type FreeA = Alt Fun

f :: String -> FreeA a
f name = liftAlt $ Fun name undefined

a, b, c, d :: FreeA a
a = f "a"
b = f "b"
c = f "c"
d = f "d"

sample :: FreeA String
sample = a <*> b

pretty :: FreeA a -> String
pretty (Alt []) = ""
pretty (Alt alts) = paren $ intercalate " <|> " $ map (paren . prettyApp) alts
  where
    prettyApp :: AltF Fun a -> String
    prettyApp (Pure _) = "0"
    prettyApp (Ap f (Alt [Pure _])) = funName f
    prettyApp (Ap f a) = funName f ++ " <*> " ++ (pretty a)

    paren :: String -> String
    paren a = "(" ++ a ++ ")"
