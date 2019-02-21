{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Web.Spock
import           Web.Spock.Config
import           Control.Monad.Trans
import           Data.IORef
import           System.Environment
import           Slide.Util
import qualified Data.Text                     as T

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

-- TODO: まとめて外部データソースに永続化する
slideIds :: [String]
slideIds = ["eb1e695c61a34f3a8cb365cde4461bf8", "f513c0c4b48b45d69dd43fe3611d24c6", "1ee303fc125842cabface3a8a00c5c8b"]

slidePageNums :: [Int]
slidePageNums = [3, 9, 11]

main :: IO ()
main = do
    port <- getEnv "PORT"
    ref      <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
    runSpock (read port) (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app = do
    get root $ text "Hello World!"
    get ("slide" <//> var) $ \slidePage ->
        let slideId = case (getSlideId slideIds slidePageNums slidePage) of
                        Just x -> x
                        Nothing -> slideIds !! 0
            page = getSlidePage slidePageNums slidePage
        in redirect $ T.pack $ toSlideLink slideId page

toSlideLink :: String -> Int -> String
toSlideLink slideId page = "https://speakerd.s3.amazonaws.com/presentations/" <> slideId <> "/preview_slide_" <> show page <>".jpg?373063"
