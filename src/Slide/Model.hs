{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Slide.Model where
import           Database.Persist.TH
import           Data.Text
import           Data.Time
import           Data.ByteString.Char8

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Slide
    sdid String
    count Int 
    deriving Show
Event
    name String
    slides [Slide]
    deriving Show

User
    name Text 
    email Text 
    password ByteString
    salt Text
    isAdmin Bool
    UniqueUserName name
    UniqueUserEmail email
    deriving Show

Session
     validUntil UTCTime
     userId UserId
     deriving Show
|]

