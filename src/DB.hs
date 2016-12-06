{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DB (
    withDB
  ) where

import Common (Upload(..))
import Control.Exception.Base (bracket, catch)
import Control.Monad (void)
import Data.Text.Lazy (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (SqlError, Connection, ConnectInfo(..), connect, close, execute_, FromRow)
import Database.PostgreSQL.Simple.Bind (bindFunction, PostgresType)
import Database.PostgreSQL.Simple.Types (Query(..))
import Settings (bindOptions)
import System.FilePath.Posix ((</>))
import System.Posix.Syslog (SyslogFn, Facility(..), Priority(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8

concat <$> mapM (bindFunction bindOptions) [
    "function get_version () returns varchar"
  ]

type instance PostgresType "varchar"     = String
type instance PostgresType "text"        = Text
type instance PostgresType "timestamptz" = UTCTime
type instance PostgresType "t_upload"    = Upload

instance FromRow Upload


include :: Connection -> String -> IO ()
include conn fn = void $ BS.readFile fn >>= (execute_ conn . Query)

checkDB :: FilePath -> SyslogFn -> Connection -> IO ()
checkDB baseDir syslog conn = do
  version <- catch (getVersion conn)
    (\(_ :: SqlError) -> do
        syslog DAEMON Warning "Schema version cannot be determined. Creating from scratch..."
        include conn (baseDir </> "init.sql")
        return "init")
  syslog DAEMON Warning (BSC8.pack $ "Schema version: " ++ version)

withDB :: forall a. FilePath -> SyslogFn -> ConnectInfo -> (Connection -> IO a) -> IO a
withDB baseDir syslog connectInfo@(ConnectInfo {..}) f = bracket acquire release process where
  acquire :: IO Connection
  acquire = do
    conn <- connect connectInfo
    syslog DAEMON Notice (BSC8.pack . concat $ [
                               "Database connection to "
                             , connectUser, "@", connectDatabase
                             , " (", connectHost, ":", show connectPort, ")"
                             , " has been established"])
    return conn

  release :: Connection -> IO ()
  release conn = do
    close conn
    syslog DAEMON Notice "Database connection has been closed"

  process :: Connection -> IO a
  process conn = do
    checkDB baseDir syslog conn
    f conn




