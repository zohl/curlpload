{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Common (
    Upload(..)
  , CurlploadSettings(..)
  , Visibility(..)
  , Expiration(..)
  , (<|>)
  ) where

import Data.Char (isAlpha)
import Data.Default (Default(..))
import Data.Time (UTCTime, NominalDiffTime)
import Data.Word (Word16)
import GHC.Generics (Generic)

(<|>) :: (Default a, Eq a) => a -> a -> a
(<|>) x y = if x == def then y else x

data Visibility
  = Public
  | Private
  | DefaultVisibility
  deriving (Eq, Show)

instance Default Visibility where
  def = DefaultVisibility

instance Read Visibility where
  readsPrec _ s = let (w, s') = break (not . isAlpha) s in case w of
    "public"  -> return (Public, s')
    "private" -> return (Private, s')
    "default" -> return (DefaultVisibility, s')
    _         -> []

data Expiration
  = ExpireAfter NominalDiffTime
  | Never
  | DefaultExpiration
  deriving (Eq, Show)

instance Default Expiration where
  def = DefaultExpiration

instance Read Expiration where
  readsPrec _ = \case
    ""      -> return (DefaultExpiration, "")
    "never" -> return (Never, "")
    s       -> case (readsPrec 0 s) :: [(Int, String)]  of
        [(n, s')] -> return (ExpireAfter . fromIntegral $ n, s')
        _         -> []

data Upload = Upload {
    uFileName        :: FilePath
  , uMimeType        :: String
  , uDispositionType :: String
  , uCreationDate    :: UTCTime
  } deriving (Eq, Show, Generic)


data CurlploadSettings = CurlploadSettings {
    csDBHost            :: String
  , csDBPort            :: Word16
  , csDBName            :: String
  , csDBUser            :: String
  , csDBPassword        :: Maybe FilePath
  , csUploadsPath       :: FilePath
  , csKeepNames         :: Bool
  , csHostName          :: String
  , csHostPort          :: Maybe Int
  , csHostLifetime      :: Maybe NominalDiffTime
  , csShareDir          :: FilePath
  , csPublicHashLength  :: Int
  , csPrivateHashLength :: Int
  , csDefaultVisibility :: Visibility
  , csDefaultExpiration :: Expiration
  } deriving (Eq, Show)
