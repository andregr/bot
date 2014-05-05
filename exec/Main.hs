{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot.Util (Only(..), printf)

main :: IO ()
main = printf "Hello, world! {}" (Only (1::Integer))