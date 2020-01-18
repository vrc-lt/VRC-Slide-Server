{-# LANGUAGE FlexibleContexts #-}
module DataStore.Internal where

import           Control.Monad.Logger           ( runStdoutLoggingT )
import           Control.Monad.Trans.Reader     ( runReaderT
                                                , ReaderT
                                                )
import           Control.Monad.Trans.Resource   ( runResourceT
                                                , MonadUnliftIO
                                                )
import           Data.Yaml.Config               ( loadYamlSettings
                                                , useEnv
                                                )
import           Database.Persist.Postgresql    ( PostgresConf(..)
                                                , withPostgresqlConn
                                                , createPostgresqlPool
                                                )
import           Database.Persist.Sql           ( Migration
                                                , ConnectionPool
                                                , BackendCompatible
                                                , SqlBackend
                                                , runMigration
                                                , runSqlPool
                                                )
import           Data.Pool

pgConf :: IO PostgresConf
pgConf = loadYamlSettings ["conf/database-setting.yml"] [] useEnv

pgPool :: IO ConnectionPool
pgPool = do
    conf <- pgConf
    runStdoutLoggingT $ createPostgresqlPool (pgConnStr conf) (pgPoolSize conf)

doMigration :: Migration -> IO ()
doMigration action = do
    conf <- pgConf
    runStdoutLoggingT
        $ runResourceT
        $ withPostgresqlConn (pgConnStr conf)
        $ runReaderT
        $ runMigration action
