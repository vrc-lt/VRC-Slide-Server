module Api.Slide where
import           Model
import           Servant
import           Servant.Auth.Server
import           Database.Persist.Sql
import           Database.Persist.Class
import           Control.Monad.IO.Class
import           DataStore.Internal
import           Data.Time.Clock
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans
import           Api.Common
import           Api.Types

dummy :: RegisteredHandler ()
dummy = undefined

handleRequestFromVRC :: Maybe Int -> UnregisteredHandler ()
handleRequestFromVRC (Just page) = undefined
handleRequestFromVRC Nothing = throwAll err404