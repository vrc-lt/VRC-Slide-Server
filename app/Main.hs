{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Web.Spock
import           Web.Spock.Config
import           Control.Monad.Trans
import           Data.IORef
import           System.Environment
import qualified Data.Text                     as T

data MySession = EmptySession
data MyAppState = CurrentSlideIndex (IORef Int)

-- TODO: まとめて外部データソースに永続化する
slideIds :: [String]
slideIds = ["88eb0ca81a99436997394b11a58c3fe4", "967f8a5f31cd4d6585e3f69c0031dd8b"]

slidePageNums :: [Int]
slidePageNums = [13, 34]

calcSlideId :: (String, Int, Int) -> (String, Int) -> (String, Int, Int)
calcSlideId  prevSlide newSlide
  | result /= "" = (result, totalNum, slidePage)
  | totalNum < slidePage = ("", totalNum, slidePage)
  | otherwise = (id, totalNum, slidePage)
  where
    (result, num, slidePage) = prevSlide
    (id, accNum) = newSlide
    totalNum = num + accNum

getSlideId :: Int -> String
getSlideId slidePage =
  let slides = zip slideIds slidePageNums
      (result, _, _) = (foldl calcSlideId ("", 0, slidePage) slides)
  in result

calcSlidePage :: (Int, Int, Int) -> Int -> (Int, Int, Int)
calcSlidePage prevSlide newSlide
  | slidePage == 0 = (0, totalNum, slidePage)
  | result /= 0 = (result, totalNum, slidePage)
  | totalNum < slidePage = (0, totalNum, slidePage)
  | otherwise = (slidePage - num, totalNum, slidePage)
  where
    (result, num, slidePage) = prevSlide
    accNum = newSlide
    totalNum = num + accNum

getSlidePage :: Int -> Int
getSlidePage slidePage =
  let (result, _, _) = (foldl calcSlidePage (0, 0, slidePage) slidePageNums)
  in result

main :: IO ()
main = do
    envs <- getEnvironment
    let (_, port) = filter (\(key, _) -> key == "PORT") envs !! 0
    ref      <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (CurrentSlideIndex ref)
    runSpock (read port) (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app = do
    get root $ text "Hello World!"
    get ("slide" <//> var) $ \slidePage ->
        let slideId = getSlideId slidePage
            page = getSlidePage slidePage
        in redirect $ T.pack $ toSlideLink slideId page

toSlideLink :: String -> Int -> String
toSlideLink slideId page = "https://speakerd.s3.amazonaws.com/presentations/" <> slideId <> "/preview_slide_" <> show page <>".jpg?373063"
