module Api.Types where
import           Servant
import           Control.Monad.Trans.Reader
import           Database.Persist.Sql
import           Model

type AdminHandler a = ReaderT (ConnectionPool, Entity User ) Handler a

type ProtectedHandler a = ReaderT (ConnectionPool, Entity User) Handler a

type AuthenticatedHandler a = ReaderT (ConnectionPool, JWTUser) Handler a

type PublicHandler a = ReaderT ConnectionPool Handler a