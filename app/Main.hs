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
slideIds = ["88eb0ca81a99436997394b11a58c3fe4", "967f8a5f31cd4d6585e3f69c0031dd8b"]

slidePageNums :: [Int]
slidePageNums = [13, 34]

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
