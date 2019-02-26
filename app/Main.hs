{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import           Data.IORef
import           System.Environment
import Network.HTTP.Types.Status
import           Database.Persist.Postgresql
                                         hiding ( get )
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Lazy as TL
import Data.Aeson
import Text.Mustache
import Database.PostgreSQL.Simple.URL
import Data.Maybe
import Database.PostgreSQL.Simple.Internal
import           Slide.Model hiding (SessionId)
import           Slide.Database
import           Slide.Util
import           Slide.Actions.User
import Data.HVect
import Crypto.BCrypt
import           Slide.Types
import           Web.Spock hiding (SessionId)
import           Web.Spock.Config



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
    spockCfg <- defaultSpockCfg (Nothing :: MySession) (PCPool pool) (DummyAppState ref)
    runSpock (read port) (spock spockCfg app)

app :: SpockM SqlBackend MySession MyAppState ()
app = do
    template <- compileMustacheDir "index" "views/"
    let render pname = TL.toStrict . renderMustache (template {templateActual = pname})
    prehook baseHook $ do
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
                Nothing -> redirect "/login"
                Just (username, password) -> 
                     do loginRes <- runSQL $ loginUser username password
                        case loginRes of
                            Just userId ->
                                do sid <- runSQL $ createSession userId
                                   writeSession (Just sid)
                                   redirect "/"
                            Nothing -> redirect "/login"
        get "register" $ html (render "register" (object []))
        post "register" $ do
            credential <- findCredential
            case credential of
                Nothing -> redirect "/register"
                Just (username, password) -> do
                    result <- runSQL $ registerUser username "" password
                    case result of
                        Right _ -> text "Register succeed."
                        Left _ -> redirect "/register"
        prehook authHook $ do
            prehook adminHook $ do
                get "admin" $ text "admin panel"

baseHook :: AppAction () (HVect '[])
baseHook = return HNil

authHook :: AppAction (HVect xs) (HVect ((UserId, User) ': xs))
authHook =
    maybeUser $ \mUser ->
    do oldCtx <- getContext
       case mUser of
         Nothing ->
             text "Unknown user. Login first!"
         Just val ->
             return (val :&: oldCtx)

data IsAdmin = IsAdmin
adminHook :: ListContains n (UserId, User) xs => AppAction (HVect xs) (HVect (IsAdmin ': xs))
adminHook =
    do (_ :: UserId, user) <- liftM findFirst getContext
       oldCtx <- getContext
       if userIsAdmin user then return (IsAdmin :&: oldCtx) else text "You don't have enough rights, sorry"

data IsAuthor = IsAuthor

maybeUser :: (Maybe (UserId, User) -> AppAction ctx a) -> AppAction ctx a
maybeUser action =
    do sess <- readSession
       case sess of
         Nothing ->
             action Nothing
         Just sid ->
             do mUser <- runSQL $ loadUser sid
                action mUser
{- initHook :: Monad m => ActionCtxT () m (HVect '[])
initHook = return HNil 
                
data IsAdmin = IsAdmin

adminHook :: (MonadIO m, ListContains n User xs) => ActionCtxT (HVect xs) m (HVect (IsAdmin ': xs))
adminHook =
    do oldCtx <- getContext
       let user = findFirst oldCtx
       if userIsAdmin user
       then return (IsAdmin :&: oldCtx)
       else text "Sorry, you are not administrator!"

authHook :: AppAction (HVect xs) (HVect ((UserId, User) ': xs))
authHook =
    maybeUser $ \mUser ->
    do oldCtx <- getContext
       case mUser of
         Nothing ->
            text "Sorry, no access!"
            noAccessPage "Unknown user. Login first!"
         Just val ->
             return (val :&: oldCtx)

{- authHook :: MonadIO m => ActionCtxT (HVect xs) m (HVect (User ': xs))
authHook =
    do oldCtx <- getContext
       sess <- readSession
       mUser <- runSQL $ getUserFromSessionId sess
       case mUser of
           Nothing -> text "Sorry, no access!"
           Just user -> return (user :&: oldCtx) -} -}

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