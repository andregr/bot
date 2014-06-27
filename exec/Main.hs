module Main where

import Bot.Run
import System.Environment

main :: IO ()
main = getArgs >>= run
