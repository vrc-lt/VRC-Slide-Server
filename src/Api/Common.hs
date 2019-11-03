{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies#-}
module Api.Common where
import           Database.Persist.Sql
import           Database.Persist.Class
import           Servant.Server
import           Servant.Auth.Server
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Control.Monad.IO.Class
import           Control.Monad.Error.Class
import           Control.Monad.Trans.Reader     ( ReaderT
                                                , ask
                                                )
import           UnliftIO                       ( MonadUnliftIO )
import           Model

doIfRegistered :: ConnectionPool -> User -> (User -> Handler a) -> Handler a
doIfRegistered pool user action = do
    mUser <- liftIO $ flip runSqlPool pool $ getByValue user
    case mUser of
        Just u  -> action $ entityVal u
        Nothing -> throwAll err401
