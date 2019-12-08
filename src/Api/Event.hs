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
import           Control.Monad
import           Api.Common
import           Api.Types


verifiedUserHook :: ConnectionPool -> JWTUser -> ProtectedHandler a -> Handler a
verifiedUserHook pool (JWTUser uid) handler = do
    muser <- liftIO $ flip runSqlPool pool $ getBy $ UniqueUid uid
    case muser of
        Just euser@(Entity _ (User _ _ _ _ True)) -> runReaderT handler (pool, entityVal euser) 
        Just euser -> throwAll err403 
        Nothing -> throwAll err401
    

getOwnedEvents :: ProtectedHandler [Event]
getOwnedEvents = do
    (pool, user) <- ask
    events <- liftIO $ flip runSqlPool pool $ selectList [EventAuthorId ==. userUid user] []
    return $ map entityVal events
    

getEvent :: Text -> ProtectedHandler Event
getEvent eventName = do
    (pool, user) <- ask
    maybeEvent <- liftIO $ flip runSqlPool pool $ getBy $ UniqueEvent eventName
    case (user, maybeEvent) of
        (user, Just (Entity _ event@(Event _ authorId _ )))
            | authorId == userUid user -> return event
            | otherwise -> throwAll err403
        _ -> throwAll err404

deleteEvent :: Text -> ProtectedHandler () 
deleteEvent eventName = do
    (pool, user) <- ask
    result <- liftIO $ flip runSqlPool pool $ do
        maybeEvent <- getBy $ UniqueEvent eventName
        case (user, maybeEvent) of
            (user, Just (Entity key (Event _ authorId _ )))
                | authorId == userUid user -> Right <$> delete key
                | otherwise -> return $ Left err403
            _ -> return $ Left err404
    case result of
        Right a -> return a
        Left err -> throwAll err

putEvent :: EventSubmission -> ProtectedHandler ()
putEvent es = do
    (pool, user) <- ask
    let ev = Event (name es) (userUid user) (slides es)
    liftIO $ flip runSqlPool pool $ do
        mEvent <- getBy $ UniqueEvent $ eventName ev
        case mEvent of
            Just e -> replace (entityKey e) ev{eventAuthorId = userUid user}
            Nothing -> void (insert ev{eventAuthorId = userUid user})
