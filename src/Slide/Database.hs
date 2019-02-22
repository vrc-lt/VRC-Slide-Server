{-# LANGUAGE TypeFamilies #-}

module Slide.Database where

import Data.Int
import Database.Persist hiding (get)
import Database.Persist.Sql
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Web.Spock hiding (get)
import Slide.Model
import Slide.Util

insertExampleEvent :: MonadIO m => ReaderT SqlBackend m (Key Event)
insertExampleEvent = insert $ Event "test" [ Slide "eb1e695c61a34f3a8cb365cde4461bf8" 3
                                           , Slide "f513c0c4b48b45d69dd43fe3611d24c6" 9
                                           , Slide "1ee303fc125842cabface3a8a00c5c8b" 11
                                           ]

getFirstEvent :: (HasSpock m, SpockConn m ~ SqlBackend) => m (Maybe (Entity Event))
getFirstEvent = runSQL $ selectFirst [] []
