
module Api.Admin where 
import           Model
import           Util
import           Api.Types
import           Servant
import           Servant.Auth.Server
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Class
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans

adminUserHook :: ConnectionPool -> JWTUser -> AdminHandler a -> Handler a
adminUserHook pool (JWTUser uid) handler = do
    muser <- liftIO $ flip runSqlPool pool $ getBy $ UniqueUid uid
    case muser of
        Just euser@(Entity _ (User _ _ _ True True)) -> runReaderT handler (pool, euser) 
        Just euser -> throwAll err403 
        Nothing -> throwAll err401

getUsers :: AdminHandler [User]
getUsers = do
    (pool, _) <- ask
    eusers <- liftIO $ flip runSqlPool pool $ selectList [] []
    return $ map entityVal eusers

getUser :: Uid -> AdminHandler User
getUser uid = do
    (pool, _) <- ask
    meuser <- liftIO $ flip runSqlPool pool $ getBy $ UniqueUid uid
    case meuser of
        Just euser -> return $ entityVal euser
        Nothing -> throwAll err404
    

putUser :: Uid -> User -> AdminHandler ()
putUser uid newUser = do
    (pool, _) <- ask
    meuser <- liftIO $ flip runSqlPool pool $ getBy $ UniqueUid uid
    case (meuser :: Maybe (Entity User)) of
        Just (Entity key _) -> liftIO $ flip runSqlPool pool $ replace key newUser
        Nothing -> throwAll err404