{-# LANGUAGE DeriveDataTypeable  #-}

module Bot.Action.StopWatch
  ( measureTime
  , averageTime
  ) where

import Bot.Types
import Bot.Util
import Data.Typeable
import Text.JSON.Generic

type ElapsedTime = Double

data AverageTime a = AverageTime a ElapsedTime
  deriving (Show, Typeable, Data)

data ApplicationKey = ApplicationKey Text [Text]
  deriving (Show, Typeable, Data)

applicationKey :: Application a -> ApplicationKey
applicationKey (Application (Command n _ _) as) = ApplicationKey n as

measureTime :: Application a -> IO b -> IO b
measureTime app action = do
  (a, t) <- stopWatch action
  updateAverageTime (applicationKey app) t
  return a

averageTime :: Application a -> IO ElapsedTime
averageTime = undefined

updateAverageTime :: ApplicationKey -> ElapsedTime -> IO ()
updateAverageTime = undefined

stopWatch :: IO a -> IO (a, ElapsedTime)
stopWatch = undefined
