
{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE TemplateHaskell #-}
module Api.User where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Trans.Reader
import           Servant
import           Database.Persist
import           Database.Persist.Sql
import           Data.Text(Text)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char                      ( toLower )
import           Model
import           Api.Types

data RegisterResult = RegSuccess | AlreadyTaken
      deriving(Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower} ''RegisterResult)

registerUser :: AuthenticatedHandler RegisterResult
registerUser = do
      (pool, user) <- ask
      liftIO $ flip runSqlPool pool $ do
            mUser <- getBy $ UniqueUid $ uid user
            case mUser of
                  Just user -> return AlreadyTaken
                  Nothing   -> insert (User (uid user) Nothing Nothing False False) >> return RegSuccess


returnDummyText :: AdminHandler Text
returnDummyText = return "hoge"
