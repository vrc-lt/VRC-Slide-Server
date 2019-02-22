{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import           Web.Spock
import           Web.Spock.Config
import           Data.IORef
import           System.Environment
import           Slide.Util
import           Database.Persist.Postgresql
                                         hiding ( get )
import           Control.Monad.Logger
import qualified Data.Text                     as T
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import Database.PostgreSQL.Simple.URL
import Data.Maybe
import Database.PostgreSQL.Simple.Internal
import           Slide.Model
import Network.HTTP.Types.Status
import           Slide.Database

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)


main :: IO ()
main = do
    port        <- getEnv "PORT"
    connStr <- getEnv "DATABASE_URL" >>= return . postgreSQLConnectionString . fromJust . parseDatabaseUrl
    ref         <- newIORef 0
    pool        <- runStdoutLoggingT $ createPostgresqlPool connStr 5

    runStdoutLoggingT
        $ runResourceT
        $ withPostgresqlConn connStr 
        $ runReaderT
        $ do 
            runMigration migrateAll 
            -- イベントの編集ができるまで暫定対応
            insertExampleEvent
    spockCfg <- defaultSpockCfg EmptySession (PCPool pool) (DummyAppState ref)
    runSpock (read port) (spock spockCfg app)

app :: SpockM SqlBackend MySession MyAppState ()
app = do
    get root $ text "Hello World!"
    get ("slide" <//> var) $ \currentPageCount-> do
        -- イベントの編集ができるまで暫定対応
        mEvent <- getFirstEvent
        case lookupSlidePage mEvent currentPageCount of
            Just (slideId, pageCount) -> redirect $ T.pack $ toSlideLink slideId pageCount
            Nothing -> setStatus notFound404

lookupSlidePage :: Maybe (Entity Event) -> Int -> Maybe (String, Int)
lookupSlidePage mEvent currentPageCount = do 
    entity <- mEvent
    let slides = eventSlides $ entityVal entity
    slideId <- getSlideId slides currentPageCount
    let page = getSlidePage (map slideCount slides) currentPageCount
    return (slideId, page)