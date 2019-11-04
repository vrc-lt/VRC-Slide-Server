module Api.Types where
import           Servant
import           Control.Monad.Trans.Reader
import           Database.Persist.Sql
import           Model

type AdminHandler a = ReaderT (ConnectionPool, User) Handler a

type RegisteredHandler a = ReaderT (ConnectionPool, User) Handler a

type UnregisteredHandler a = ReaderT ConnectionPool Handler a