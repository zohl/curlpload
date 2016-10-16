{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Config (
    bindOptions
  ) where

import Data.Default (def)
import Database.PostgreSQL.Simple.Bind (PostgresBindOptions(..), PGFunction(..))
import Text.CaseConversion (convertCase, WordCase(..))

mkFunctionName :: PGFunction -> String
mkFunctionName (PGFunction _schema name _args _result) = convertCase Snake Camel $ name

bindOptions :: PostgresBindOptions
bindOptions = (def :: PostgresBindOptions) {
    pboFunctionName = mkFunctionName
  }
