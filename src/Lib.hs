{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( startApp
    , app
    ) where

import Protolude hiding(fromStrict, readFile)
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text, words, pack)
import Data.ByteString (ByteString, readFile)
import Data.ByteString.Lazy (fromStrict)
import Data.List (find, lookup)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server
import Servant.Auth.Server
import Crypto.JWT (SignedJWT, JWTError, ClaimsSet, stringOrUri, decodeCompact, defaultJWTValidationSettings, verifyClaims, claimSub, FromCompact, AsError, StringOrURI, JWTValidationSettings)
import Crypto.JOSE.JWK (JWK, fromOctets, JWKSet(..))
import Crypto.JOSE.JWA.JWS (Alg(..))
import Control.Monad (join)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans
import Control.Monad.Except (catchError)
import Control.Lens ((^.))
import Database.Persist.Sql
import DataStore.Internal
import Model
import Api.User
import Api.Slide
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import           Data.Time.Clock


type API auths  = (Servant.Auth.Server.Auth auths User :> AdminAPI) :<|> (Servant.Auth.Server.Auth auths User :> ProtectedAPI) :<|> PublicAPI

isAdmin :: User -> Bool
isAdmin = (== "hoge") . userUid


type AdminAPI = "admin" :> "events" :> Get '[JSON] Text

adminApi :: Proxy AdminAPI
adminApi = Proxy

adminServer :: ConnectionPool -> Servant.Auth.Server.AuthResult User -> ServerT AdminAPI Handler
adminServer pool (Servant.Auth.Server.Authenticated user) = do 
                                                              mUser <- liftIO $ flip runSqlPool pool $ getByValue user
                                                              case mUser of
                                                                    Just (Entity _ (usr@(User _ _ _ True True))) -> hoistServer adminApi (`runReaderT` (pool, usr)) $ returnDummyText
                                                                    Just _ -> throwAll err403
                                                                    Nothing   -> throwAll err401
adminServer _ _ =  throwAll err401

type ProtectedAPI = "api" :> "slides" :> Get '[JSON] ()

protectedApi :: Proxy ProtectedAPI
protectedApi = Proxy

protected :: ConnectionPool -> Servant.Auth.Server.AuthResult User -> ServerT ProtectedAPI Handler
protected pool (Servant.Auth.Server.Authenticated user) = do
                                                              mUser <- liftIO $ flip runSqlPool pool $ getByValue user
                                                              case mUser of
                                                                    Just (Entity _ (usr@(User _ _ _ _ True))) -> hoistServer protectedApi (`runReaderT` (pool, usr)) $ dummy
                                                                    Just _ -> throwAll err403
                                                                    Nothing   -> throwAll err401
protected _ _ =  throwAll err401

type PublicAPI = "slide" :> Capture "event" Text :> Capture "page" Int :> Get '[JSON] ()

publicApi :: Proxy PublicAPI
publicApi = Proxy

publicServer :: ConnectionPool -> Server PublicAPI
publicServer pool = hoistServer publicApi (`runReaderT` pool) $ handleRequestFromVRC

data AuthResult val
  = BadPassword
  | NoSuchUser
  | Authenticated val
  | Indefinite

startApp :: IO ()
startApp = do
  jsonJwk <- fetchKey
  let Just (Success jwkset) = fromJSON <$> decode jsonJwk
  let jwk = fromOctets jsonJwk
  Just trustedAudiences <- decode . fromStrict <$> readFile "./audience.json"
  let jwtCfg = JWTSettings jwk (Just RS256) jwkset (matchAud trustedAudiences)
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      --- Here we actually make concrete
      api = Proxy :: Proxy (API '[JWT])
      matchAud :: [StringOrURI] -> StringOrURI -> IsMatch
      matchAud trusteds aud = case find (== aud) trusteds of
                                Just _ -> Matches
                                Nothing -> DoesNotMatch
  doMigration migrateAll
  pool <- pgPool
  putStrLn ("starting server at port 8080" :: Text)
  run 8080 $ app pool cfg defaultCookieSettings jwtCfg
  where
    fetchKey = do
      manager <- HTTP.newManager tlsManagerSettings
      request <- HTTP.parseRequest "https://www.googleapis.com/oauth2/v3/certs"
      response <- HTTP.httpLbs request manager
      return $ HTTP.responseBody response

app :: ConnectionPool -> Context '[CookieSettings, JWTSettings] -> CookieSettings -> JWTSettings -> Application
app pool cfg cookieSettings jwtCfg = serveWithContext api cfg (server pool cookieSettings jwtCfg)

api :: Proxy (API '[JWT])
api = Proxy

server :: ConnectionPool -> CookieSettings -> JWTSettings -> Server (API auths)
server pool cs jwts = adminServer pool :<|> protected pool :<|> publicServer pool


