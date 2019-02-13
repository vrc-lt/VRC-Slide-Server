{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Web.Spock
import           Web.Spock.Config
import           Control.Monad.Trans
import           Data.IORef
import System.Environment
import qualified Data.Text                     as T

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main = do
    envs <- getEnvironment
    let (_, port) = filter (\(key, _) -> key == "PORT") envs !! 0
    ref      <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
    runSpock (read port) (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app = do
    get root $ text "Hello World!"
    get ("hello" <//> var) $ \name -> do
        (DummyAppState ref) <- getState
        visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i + 1, i + 1)
        text
            ("Hello " <> name <> ", you are visitor number " <> T.pack
                (show visitorNumber)
            )
    get ("slide" <//> var) $ \slidePage -> 
        redirect $ T.pack $ slides slidePage

slides :: Int -> String
slides page = undefined