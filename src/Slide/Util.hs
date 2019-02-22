{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Slide.Util where
import Database.Persist.Sql
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Web.Spock
import Slide.Model

calcSlideId :: [Slide] -> Int -> Maybe String
calcSlideId ((Slide slideId count):xs) pageCount
  | count <= pageCount = calcSlideId xs (pageCount - count)
  | count > pageCount = Just slideId
calcSlideId [] _ = Nothing

getSlideId :: [Slide] -> Int -> Maybe String
getSlideId slides slidePage = calcSlideId slides slidePage

calcSlidePage :: [Int] -> Int -> Int
calcSlidePage (count:xs) pageCount
  | count <= pageCount = calcSlidePage xs (pageCount - count)
  | count > pageCount = pageCount
calcSlidePage [] _ = 0

getSlidePage :: [Int] -> Int -> Int
getSlidePage slidePageNums slidePage = calcSlidePage slidePageNums slidePage

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSQL action =
    runQuery $ \conn ->
        runResourceT $ runNoLoggingT $ runSqlConn action conn
{-# INLINE runSQL #-}

toSlideLink :: String -> Int -> String
toSlideLink slideId page =
    "https://speakerd.s3.amazonaws.com/presentations/"
        <> slideId
        <> "/preview_slide_"
        <> show page
        <> ".jpg?373063"