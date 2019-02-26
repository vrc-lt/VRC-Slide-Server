{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import           Web.Spock hiding (SessionId)
import           Web.Spock.Config
import           Data.IORef
import           System.Environment
import Network.HTTP.Types.Status
import           Database.Persist.Postgresql
                                         hiding ( get )
import           Control.Monad.Logger
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Lazy as TL
import Data.Aeson
import Text.Mustache
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import Database.PostgreSQL.Simple.URL
import Data.Maybe
import Database.PostgreSQL.Simple.Internal
import           Slide.Model
import           Slide.Database
import           Slide.Util
import Data.HVect
import Control.Monad.IO.Class
import           Slide.User
import Crypto.BCrypt

type MySession = Maybe SessionId 
data MyAppState = DummyAppState (IORef Int)
type AppAction ctx a = SpockActionCtx ctx SqlBackend MySession MyAppState



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
    spockCfg <- defaultSpockCfg Nothing (PCPool pool) (DummyAppState ref)
    runSpock (read port) (spock spockCfg app)

app :: SpockM SqlBackend MySession MyAppState ()
app = do
    template <- compileMustacheDir "index" "views/"
    let render pname = TL.toStrict . renderMustache (template {templateActual = pname})
    get root $ text "Hello World!"
    get ("slide" <//> var) $ \currentPageCount-> do
        -- イベントの編集ができるまで暫定対応
        mEvent <- runSQL getFirstEvent
        case lookupSlidePage mEvent currentPageCount of
            Just (slideId, pageCount) -> redirect $ T.pack $ toSlideLink slideId pageCount
            Nothing -> setStatus notFound404
    get "login" $ html (render "login" (object []))
    post "login" $ do
        credential <- findCredential
        case credential of
            Nothing -> setStatus status400 >> text "Missing parameter."
            Just (username, password) -> do
                user <- runSQL $ getBy $ UniqueUserName username
                case user of
                    Just entity -> 
                        let password' = TE.encodeUtf8 password
                        in if validatePassword (userPassword (entityVal entity)) password' 
                            then text "Login succeed."
                            else setStatus status400 >> text "Wrong parameter."
                    Nothing -> setStatus status400 >> text "Wrong parameter."

                
findCredential :: MonadIO m => ActionCtxT ctx m (Maybe (T.Text, T.Text))
findCredential = do
  username <- param "username"
  password <- param "password"
  pure $ (,) <$> username <*> password

lookupSlidePage :: Maybe Event -> Int -> Maybe (String, Int)
lookupSlidePage mEvent currentPageCount = do 
    event <- mEvent
    let slides = eventSlides event
    slideId <- getSlideId slides currentPageCount
    let page = getSlidePage (map slideCount slides) currentPageCount
    return (slideId, page)