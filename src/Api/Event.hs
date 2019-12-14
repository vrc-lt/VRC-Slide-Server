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
import           Data.UUID (UUID)
import           Data.UUID.V4 (nextRandom)
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
    

getEvent :: UUID -> ProtectedHandler Event
getEvent eventId = do
    (pool, user) <- ask
    maybeEvent <- liftIO $ flip runSqlPool pool $ getBy $ UniqueEvent eventId
    case (user, maybeEvent) of
        (user, Just (Entity _ event@(Event _ _ authorId _ )))
            | authorId == userUid user -> return event
            | otherwise -> throwAll err403
        _ -> throwAll err404

deleteEvent :: UUID -> ProtectedHandler () 
deleteEvent eventId = do
    (pool, user) <- ask
    result <- liftIO $ flip runSqlPool pool $ do
        maybeEvent <- getBy $ UniqueEvent eventId
        case (user, maybeEvent) of
            (user, Just (Entity key (Event _ _ authorId _ )))
                | authorId == userUid user -> Right <$> delete key
                | otherwise -> return $ Left err403
            _ -> return $ Left err404
    case result of
        Right a -> return a
        Left err -> throwAll err

modifyEvent :: UUID -> EventSubmission -> ProtectedHandler Event
modifyEvent eventId (EventSubmission ename eslides) = do
    (pool, user) <- ask
    result <- liftIO $ flip runSqlPool pool $ do
        mEvent <- getBy $ UniqueEvent eventId
        case mEvent of 
            Just (Entity key event) 
                | eventAuthorId event == userUid user -> do
                    let newEvent = Event eventId ename (eventAuthorId event) eslides
                    replace key newEvent 
                    return (Right newEvent)
                | otherwise -> return $ Left err403
            Nothing -> return $ Left err404
    case result of
        Right event -> return event
        Left e -> throwAll e



newEvent :: EventSubmission -> ProtectedHandler Event
newEvent es = do
    (pool, user) <- ask
    uuid <- liftIO nextRandom 
    let ev = Event uuid (name es) (userUid user) (slides es)
    liftIO $ flip runSqlPool pool $ insert ev >> return ev
