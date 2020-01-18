{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Util where
import           Database.Persist.Sql
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Model
import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T

calcSlideId :: [Slide] -> Int -> Maybe T.Text
calcSlideId (Slide sdId count : xs) pageCount
  | count <= pageCount = calcSlideId xs (pageCount - count)
  | count > pageCount  = Just sdId
calcSlideId [] _ = Nothing

getSlideId :: [Slide] -> Int -> Maybe T.Text
getSlideId = calcSlideId 

calcSlidePage :: [Int] -> Int -> Int
calcSlidePage (count : xs) pageCount
  | count <= pageCount = calcSlidePage xs (pageCount - count)
  | count > pageCount  = pageCount
calcSlidePage [] _ = 0

getSlidePage :: [Int] -> Int -> Int
getSlidePage = calcSlidePage 
