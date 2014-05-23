module Bot.Types where

data Text
newtype Error = Error 
  { errorMsg :: Text 
  }
data Config = Config 
  { configCommands :: [Command] 
  }
data Result = Failure | Success
newtype Action = Action 
  { runAction :: IO Result 
  }
newtype Parser a = Parser 
  { parse :: [Text] -> Either Error a
  }
data Command = Command 
  { prepareAction :: Parser Action
  , commandName :: Text 
  , commandDesc :: Text
  }


