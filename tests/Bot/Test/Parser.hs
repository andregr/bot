{-# LANGUAGE OverloadedStrings #-}

module Bot.Test.Parser ( tests ) where

import Bot.Test.Util
import Test.Framework.TH              ( testGroupGenerator )
import Test.Framework.Providers.HUnit ( testCase ) -- Used by TH
import Test.HUnit                     ( assertEqual )

func :: IO ()
func = putStrLn "Hello out"

case_parser =
  captureStdOut func $ \(_, output) -> do
    assertEqual "" "Hello out\n" output 

tests = $(testGroupGenerator)