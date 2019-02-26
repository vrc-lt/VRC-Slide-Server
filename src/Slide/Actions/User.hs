
{-# LANGUAGE OverloadedStrings #-}
module Slide.Actions.User where


import Slide.Model
import Slide.Util
import System.Random
import Database.Persist
import Database.Persist.Sql
import Data.Word8
import Control.Monad
import Data.Time
import Control.Monad.Trans
import qualified Data.ByteString as BS
import qualified Crypto.Hash.SHA512 as SHA
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


randomBytes:: Int -> StdGen -> [Word8]
randomBytes 0 _ = []
randomBytes ct g =
    let (value, nextG) = next g
    in fromIntegral value:randomBytes (ct - 1) nextG

randomBS :: Int -> StdGen -> BS.ByteString
randomBS len g =
    BS.pack $ randomBytes len g

hashPassword :: T.Text -> BS.ByteString -> BS.ByteString
hashPassword password salt =
     SHA.finalize $ SHA.updates SHA.init [salt, T.encodeUtf8 $ password]

createSession :: UserId -> SqlPersistM SessionId
createSession userId =
    do now <- liftIO getCurrentTime
       gen <- liftIO getStdGen
       insert (Session (T.decodeUtf8 $ randomBS 10 gen) (addUTCTime (5 * 3600) now) userId)

killSessions :: UserId -> SqlPersistM ()
killSessions userId =
    deleteWhere [ SessionUserId ==. userId ]

loginUser :: T.Text -> T.Text -> SqlPersistM (Maybe UserId)
loginUser username password =
    do mUserU <- getBy (UniqueUserName username)
       mUserE <- getBy (UniqueUserEmail username)
       case mUserU `mplus` mUserE of
         Just userEntity ->
             let user = entityVal userEntity
             in if userPassword user == (makeHex $ hashPassword password (decodeHex $ userSalt user))
                then return $ Just (entityKey userEntity)
                else return Nothing
         Nothing ->
             return Nothing

loadUser :: SessionId -> SqlPersistM (Maybe (UserId, User))
loadUser sessId =
    do mSess <- get sessId
       now <- liftIO getCurrentTime
       case mSess of
         Just sess | sessionValidUntil sess > now ->
             do mUser <- get (sessionUserId sess)
                return $ fmap (\user -> (sessionUserId sess, user)) mUser
         _ ->
             return Nothing


registerUser :: T.Text -> T.Text -> T.Text -> SqlPersistM (Either String String)
registerUser username email password =
    do mUserU <- getBy (UniqueUserName username)
       mUserE <- getBy (UniqueUserEmail email)
       case (mUserU :: Maybe (Entity User), mUserE :: Maybe (Entity User)) of
         (Just _, _) ->
             return (Left "Username already taken!")
         (_, Just _) ->
             return (Left "Email already registered!")
         (Nothing, Nothing) ->
             do g <- liftIO $ getStdGen
                let salt = randomBS 512 g
                    hash = hashPassword password salt
                _ <- insert (User username email (makeHex hash) (makeHex salt) False)
                return (Right "Signup complete. You may now login.")