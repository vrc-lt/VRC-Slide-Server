{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Slide.Model where
import           Database.Persist.TH
import           Data.Text
import           Data.Time
import           Data.Aeson()


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Slide json
    sdid String
    count Int 
    deriving Show
Event json
    name String
    slides [Slide]
    UniqueEvent name
    deriving Show

User
    name Text 
    email Text 
    password Text
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

