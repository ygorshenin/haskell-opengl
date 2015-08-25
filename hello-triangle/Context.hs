module Context (Context (..), defaultContext) where

data Context = Context { ctxShutdown :: Bool }

defaultContext :: Context
defaultContext = Context { ctxShutdown = False }
