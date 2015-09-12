module Context (Context (..), defaultContext) where

data Context = Context { ctxQuit :: Bool } deriving (Show, Eq)

defaultContext :: Context
defaultContext = Context { ctxQuit = False }
