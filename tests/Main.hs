module Main ( main ) where

import qualified Bot.Test.TestIntegration
import qualified Bot.Test.TestParser
import Test.Framework                 ( defaultMain )

main = defaultMain [ Bot.Test.TestIntegration.tests 
                   , Bot.Test.TestParser.tests
                   ]