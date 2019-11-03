{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import Protolude hiding(fromStrict, readFile, get)
import Lib (app)
import Data.Aeson
import Data.List (find, lookup)
import Data.ByteString (ByteString, readFile)
import Data.ByteString.Lazy (fromStrict)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Crypto.JOSE.JWK (JWK, fromOctets, JWKSet(..))
import Crypto.JOSE.JWA.JWS (Alg(..))
import Servant.Server
import Servant.Auth.Server
import Crypto.JWT (SignedJWT, JWTError, ClaimsSet, stringOrUri, decodeCompact, defaultJWTValidationSettings, verifyClaims, claimSub, FromCompact, AsError, StringOrURI, JWTValidationSettings)

-- CIの環境でファイル読み書きできるまではビルド通るかだけチェック
main :: IO ()
main = return ()
{-
    
    jsonJwk <- readFile "./jwk.json" 
    let Just (Success jwkset) = fromJSON <$> decode (fromStrict jsonJwk)
    let jwk = fromOctets jsonJwk
    Just trustedAudiences <- decode . fromStrict <$> readFile "./audience.json"
    let jwtCfg = JWTSettings jwk (Just RS256) jwkset (matchAud trustedAudiences)
        cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
        --- Here we actually make concrete
        matchAud :: [StringOrURI] -> StringOrURI -> IsMatch
        matchAud trusteds aud = case find (== aud) trusteds of
                                    Just _ -> Matches
                                    Nothing -> DoesNotMatch
    hspec $ spec cfg defaultCookieSettings jwtCfg

spec :: Context '[CookieSettings, JWTSettings] -> CookieSettings -> JWTSettings -> Spec
spec context cookieCfg jwtCfg = with (return (app context cookieCfg jwtCfg)) $ do
    describe "GET /users" $ do
        it "responds with 200" $ do
            get "/users" `shouldRespondWith` 200
    describe "GET /protected" $ do
        it "responds with 401" $ do
            get "/protected" `shouldRespondWith` 401
    -}