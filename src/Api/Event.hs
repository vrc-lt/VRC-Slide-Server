{-# LANGUAGE NoImplicitPrelude #-}

module Api.Event where

import Protolude hiding(fromStrict, readFile, ask, concat, get, replace)
import           Model
import           Util
import           Servant
import           Servant.Auth.Server
import           Database.Persist
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


verifiedUserHook :: ConnectionPool -> JWTUser -> ProtectedHandler a -> Handler a
verifiedUserHook pool (JWTUser uid) handler = do
    muser <- liftIO $ flip runSqlPool pool $ getBy $ UniqueUid uid
    case muser of
        Just euser@(Entity _ (User _ _ _ _ True)) -> runReaderT handler (pool, euser) 
        Just euser -> throwAll err403 
        Nothing -> throwAll err401
    

getOwnedEvents :: ProtectedHandler [Event]
getOwnedEvents = do
    (pool, euser) <- ask
    events <- liftIO $ flip runSqlPool pool $ selectList [EventAuthorId ==. entityKey euser] []
    return $ map entityVal events
    

getEvent :: Text -> ProtectedHandler Event
getEvent eventName = do
    (pool, euser) <- ask
    maybeEvent <- liftIO $ flip runSqlPool pool $ getBy $ UniqueEvent eventName
    case (euser, maybeEvent) of
        (euser, (Just (Entity _ event@(Event _ authorId _ ))))
            | authorId == entityKey euser -> return event
            | otherwise -> throwAll err403
        _ -> throwAll err404

putEvent :: Text -> Event -> ProtectedHandler ()
putEvent eventName ev = do
    (pool, euser) <- ask
    liftIO $ flip runSqlPool pool $ do
        mEvent <- getBy $ UniqueEvent eventName
        case mEvent of
            Just e -> replace (entityKey e) ev{eventAuthorId = entityKey euser}
            Nothing -> insert ev{eventAuthorId = entityKey euser} >> return ()