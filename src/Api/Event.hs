{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Api.Event where

import Protolude hiding(fromStrict, readFile, ask, concat, get)
import           Model
import           Util
import           Servant
import           Servant.Auth.Server
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Class
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           DataStore.Internal
import           Data.Time.Clock
import           Data.Text (Text, concat, pack)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString  as BS
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans
import           Api.Common
import           Api.Types


verifiedUserHook :: ProtectedHandler a -> ReaderT (ConnectionPool, JWTUser) Handler a
verifiedUserHook handler = do
    (pool, JWTUser uid) <- ask
    muser <- liftIO $ flip runSqlPool pool $ getBy $ UniqueUid uid
    case muser of
        Just (euser@(Entity _ (User _ _ _ _ True))) -> lift $ runReaderT handler (pool, euser) 
        Just euser -> throwAll err403 
        Nothing -> throwAll err401
    

getOwnedEvents :: ProtectedHandler [Event]
getOwnedEvents = do
    (pool, euser) <- ask
    events <- liftIO $ flip runSqlPool pool $ do
        eventsList <- selectList [EventAuthorId ==. entityKey euser] []
        return $ eventsList 
    return $ map entityVal events
    

getEvent :: Text -> ProtectedHandler Event
getEvent eventName = do
    (pool, euser) <- ask
    maybeEvent <- liftIO $ flip runSqlPool pool $ getBy $ UniqueEvent eventName
    case (euser, maybeEvent) of
        (euser, (Just (Entity _ event@(Event _ authorId _ ))))
            | authorId == entityKey euser -> return event
            | otherwise -> throwAll err403
        otherwise -> throwAll err404