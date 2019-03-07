
module Slide.Types where 
import Slide.Model
import Data.IORef
import Database.Persist.Sql
import Web.Spock hiding(SessionId)

type MySession = Maybe SessionId
data MyAppState = DummyAppState (IORef Int)
type AppAction ctx = SpockActionCtx ctx SqlBackend MySession MyAppState