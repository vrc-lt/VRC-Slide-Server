{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Web.Spock
import           Web.Spock.Config
import           Control.Monad.Trans
import           Data.IORef
import           System.Environment
import qualified Data.Text                     as T

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

-- TODO: まとめて外部データソースに永続化する
slideIds :: [String]
slideIds = ["88eb0ca81a99436997394b11a58c3fe4", "967f8a5f31cd4d6585e3f69c0031dd8b"]

slidePageNums :: [Int]
slidePageNums = [13, 34]

calcSlideId :: [(String, Int)] -> Int -> Maybe String
calcSlideId ((slideId, count):xs) pageCount
  | count <= pageCount = calcSlideId xs (pageCount - count)
  | count > pageCount = Just slideId
calcSlideId [] _ = Nothing

getSlideId :: [String] -> [Int] -> Int -> Maybe String
getSlideId slideIds slidePageNums slidePage =
  let slides = zip slideIds slidePageNums
  in calcSlideId slides slidePage

calcSlidePage :: [Int] -> Int -> Int
calcSlidePage (count:xs) pageCount
  | count <= pageCount = calcSlidePage xs (pageCount - count)
  | count > pageCount = pageCount
calcSlidePage [] _ = 0

getSlidePage :: [Int] ->Int -> Int
getSlidePage slidePageNums slidePage = calcSlidePage slidePageNums slidePage

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
    get ("slide" <//> var) $ \slidePage ->
        let slideId = case (getSlideId slideIds slidePageNums slidePage) of
                        Just x -> x
                        Nothing -> slideIds !! 0
            page = getSlidePage slidePageNums slidePage
        in redirect $ T.pack $ toSlideLink slideId page

toSlideLink :: String -> Int -> String
toSlideLink slideId page = "https://speakerd.s3.amazonaws.com/presentations/" <> slideId <> "/preview_slide_" <> show page <>".jpg?373063"
