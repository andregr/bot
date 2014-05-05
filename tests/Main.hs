module Main ( main ) where

import qualified Bot.Test.Integration
import qualified Bot.Test.Parser
import Test.Framework                 ( defaultMain )

main = defaultMain [ Bot.Test.Integration.tests 
                   , Bot.Test.Parser.tests
                   ]