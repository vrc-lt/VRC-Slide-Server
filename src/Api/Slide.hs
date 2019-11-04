{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Api.Slide (dummy, handleRequestFromVRC) where

import Protolude hiding(fromStrict, readFile, ask, concat)
import           Model
import           Util
import           Servant
import           Servant.Auth.Server
import           Database.Persist.Sql
import           Database.Persist.Class
import           Control.Monad.IO.Class
import           DataStore.Internal
import           Data.Time.Clock
import           Data.Text (Text, concat, pack)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString  as BS
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans
import           Api.Common
import           Api.Types

dummy :: RegisteredHandler ()
dummy = undefined

handleRequestFromVRC :: Text -> Int -> UnregisteredHandler ()
handleRequestFromVRC eventName currentPageCount = do
    pool <- ask
    event <- liftIO $ flip runSqlPool pool $ getBy $ UniqueEvent eventName
    case lookupSlidePage (event >>= return . entityVal) currentPageCount of
        Just (slideId, pageCount) -> throwAll $ err303 {errHeaders = [("Location", (encodeUtf8 $ toSlideLink slideId pageCount))]} 
        Nothing -> throwAll err404

lookupSlidePage :: Maybe Event -> Int -> Maybe (Text, Int)
lookupSlidePage mEvent currentPageCount = do
    event <- mEvent
    let slides = eventSlides event
    slideId <- getSlideId slides currentPageCount
    let page = getSlidePage (map slideCount slides) currentPageCount
    return (slideId, page)

toSlideLink :: Text -> Int -> Text
toSlideLink slideId page =
   concat ["https://speakerd.s3.amazonaws.com/presentations/",
   slideId,
   "/preview_slide_",
   (pack $ show page),
   ".jpg?373063"]