{-# LANGUAGE OverloadedStrings #-}
module Slide.Util where

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
