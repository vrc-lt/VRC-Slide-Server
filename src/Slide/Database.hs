{-# LANGUAGE TypeFamilies #-}

module Slide.Database where


import           Web.Spock()
import           Database.Persist
import           Database.Persist.Sql
import           Data.Text
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class
import Data.ByteString
import           Slide.Model
import           Data.Time

insertExampleEvent :: MonadIO m => ReaderT SqlBackend m ()
insertExampleEvent = do 
    mEvent <- getFirstEvent
    case mEvent of
        Just _ -> return ()
        Nothing -> repsert
            (toSqlKey 1)
            (Event
                "test"
                [ Slide "eb1e695c61a34f3a8cb365cde4461bf8" 3
                , Slide "f513c0c4b48b45d69dd43fe3611d24c6" 9
                , Slide "1ee303fc125842cabface3a8a00c5c8b" 11
                ]
            )

getFirstEvent :: MonadIO m => ReaderT SqlBackend m (Maybe Event)
getFirstEvent = get $ toSqlKey 1

getUserFromSessionId :: MonadIO m => Text -> ReaderT SqlBackend m (Maybe User)
getUserFromSessionId sessionId = do
    mSession <- getBy $ UniqueSessionId sessionId
    case mSession of
        Just session -> get $ sessionUserId $ entityVal session 
        Nothing -> return Nothing

findSession :: MonadIO m => Text -> ReaderT SqlBackend m (Maybe Session)
findSession sessionId = do
    mSession <- getBy $ UniqueSessionId sessionId
    now <- liftIO getCurrentTime
    case mSession of
        Just sess -> 
            if (sessionValidUntil $ entityVal sess) > now
            then return $ Just $ entityVal sess
            else return Nothing
        Nothing -> return Nothing
    