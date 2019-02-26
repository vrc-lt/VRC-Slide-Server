
module Slide.Types where 
import Data.IORef
import Data.Text
import Database.Persist.Sql
import Web.Spock

type MySession = Maybe SessionId
data MyAppState = DummyAppState (IORef Int)
type AppAction ctx = SpockActionCtx ctx SqlBackend MySession MyAppState