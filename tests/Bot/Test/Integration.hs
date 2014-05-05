{-# LANGUAGE OverloadedStrings #-}

module Bot.Test.Integration ( tests ) where

import Bot.Test.Util
import Test.Framework.TH              ( testGroupGenerator )
import Test.Framework.Providers.HUnit ( testCase ) -- Used by TH
import Test.HUnit                     ( assertEqual )

func :: IO ()
func = putStrLn "Hello out"

case_integration = do 
  captureStdOut func $ \(_, output) -> do
    assertEqual "" "Hello out\n" output 

tests = $(testGroupGenerator)