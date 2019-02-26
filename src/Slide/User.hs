{-# LANGUAGE OverloadedStrings #-}
module Slide.User where


import Database.Persist
import Database.Persist.Sql
import Crypto.BCrypt
import Control.Monad
import Data.Time
import Control.Monad.Trans
import qualified Data.ByteString as BS 
import qualified Data.ByteString.Char8 as BS8(pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Slide.Model
import Slide.Util

data CommonResponse = CommonSuccess String | CommonError String

registerUser :: T.Text -> T.Text -> T.Text -> SqlPersistM CommonResponse
registerUser username email password =
    do mUserU <- getBy (UniqueUserName username)
       mUserE <- getBy (UniqueUserEmail email)
       case (mUserU, mUserE) of
         (Just _, _) ->
             return (CommonError "Username already taken!")
         (_, Just _) ->
             return (CommonError "Email already registered!")
         (Nothing, Nothing) ->
             do --g <- liftIO $ getStdGen
                mHash <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (BS8.pack $ T.unpack password)
                case mHash of
                    Just hash -> do
                        _ <- insert (User username email hash "" False)
                        return (CommonSuccess "Signup complete. You may now login.")
                    Nothing -> return(CommonError "Hash failed")
