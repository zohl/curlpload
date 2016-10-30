{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HTTP (
    ContentDisposition(..)
  , hContentDisposition
  , parseContentDisposition
  , formatContentDisposition
  , processBody
  ) where

import Control.Applicative (liftA2, (<|>))
import Data.Attoparsec.ByteString (Parser, parseOnly) 
import Data.Attoparsec.ByteString.Char8 (takeWhile1, notInClass, char, skipSpace)
import Data.Maybe (fromMaybe)
import Network.HTTP.Types (HeaderName)
import Network.Wai (Request(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8

-- TODO import from http-types
hContentDisposition :: HeaderName
hContentDisposition = "Content-Disposition"


data ContentDisposition = ContentDisposition {
    cdType     :: String
  , cdFilename :: FilePath                       
  } deriving (Eq, Show)

  
parseContentDisposition :: BS.ByteString -> Maybe ContentDisposition
parseContentDisposition = either (const Nothing) Just . (parseOnly header) where
  header :: Parser ContentDisposition 
  header = do
    _ <- skipSpace
    cdType <- BSC8.unpack <$> (takeWhile1 $ notInClass " ;")
    cdFilename <- (BSC8.unpack . fromMaybe undefined . lookup "filename") <$> params
    return $ ContentDisposition {..}

    
  params :: Parser [(BS.ByteString, BS.ByteString)]
  params = (liftA2 (:) param params) <|> (return [])

  param :: Parser (BS.ByteString, BS.ByteString)
  param = do
    _ <- skipSpace <* char ';' <* skipSpace
    name <- (takeWhile1 (notInClass " ="))
    _ <- skipSpace <* char '=' <* skipSpace
    value <- ((char '"') *> (takeWhile1 (/= '"')) <* (char '"')) <|> (takeWhile1 (/= ';'))
    return (name, value)
    
    
formatContentDisposition :: ContentDisposition -> BS.ByteString
formatContentDisposition (ContentDisposition {..}) = BSC8.pack . concat $ [
    cdType, "; filename=\"", cdFilename, "\""
  ]


processBody :: Request -> (BS.ByteString -> IO a) -> IO [a]
processBody request f = requestBody request >>= \result ->
  if (result == BS.empty)
  then return []
  else liftA2 (:) (f result) (processBody request f)
                                              
