{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DB (
    checkDB
  , withDB
  ) where

import Common (Upload(..))
import Control.Applicative ((<|>))
import Control.Exception.Base (bracket, catch)
import Control.Monad (void)
import Data.Attoparsec.ByteString (parseOnly, many')
import Data.Attoparsec.ByteString.Char8 (char, notChar)
import Data.Text.Lazy (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.PostgreSQL.Simple (SqlError, Connection, ConnectInfo(..), connect, close, execute_)
import Database.PostgreSQL.Simple.Bind (bindFunction, PostgresType)
import Database.PostgreSQL.Simple.FromField (FromField(..), ResultError(..), returnError, typename)
import Database.PostgreSQL.Simple.Types (Query(..))
import Settings (bindOptions)
import qualified Data.ByteString as BS


concat <$> mapM (bindFunction bindOptions) [
    "function get_version () returns varchar"
  ]


type instance PostgresType "varchar"     = String
type instance PostgresType "text"        = Text
type instance PostgresType "timestamptz" = UTCTime
type instance PostgresType "t_upload"    = Upload




withDB :: ConnectInfo -> (Connection -> IO a) -> IO a
withDB connectInfo = bracket (connect connectInfo) close

include :: Connection -> String -> IO ()
include conn fn = void $ BS.readFile fn >>= (execute_ conn . Query)

checkDB :: Connection -> IO ()
checkDB conn = do
  version <- catch
    (getVersion conn)
    (\(_ :: SqlError) -> include conn "db/init.sql" >> return "init")

  putStrLn version


-- TODO rewrite with generic parser
instance FromField Upload where
  fromField f v = checkType parseValue where

    checkType cb = (("t_upload" /=) <$> typename f) >>= \b -> case b of
      True -> returnError Incompatible f ""
      False -> ($ v) $ maybe (returnError UnexpectedNull f "") cb

    parseValue bs = ($ (parseOnly value bs)) $ either
      (returnError ConversionFailed f) pure

    value = do
      _                <- char '('
      uFileName        <- postgresString
      _                <- char ','
      uMimeType        <- postgresString
      _                <- char ','
      uDispositionType <- postgresString
      _                <- char ','
      _tmp             <- postgresString
      _                <- char ')'
      let uCreationDate = posixSecondsToUTCTime 0
      return $ Upload {..}

    postgresString = ((char '"') *> (many' $ notChar '"') <* (char '"')) <|> (many' $ notChar ',')


