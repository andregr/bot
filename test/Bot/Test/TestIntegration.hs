{-# LANGUAGE OverloadedStrings #-}

module Bot.Test.TestIntegration ( tests ) where

import Bot.Test.TestUtil

func :: IO ()
func = putStrLn "Hello out"

case_integration = do 
  captureStdOut func $ \(_, output) -> do
    "Hello out\n" @?= output 

tests = $(testGroupGenerator)