{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Config(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

data Config = Config { channel :: String, secret :: String, host :: String, port :: Int, obs_host :: String, obs_port :: Int, update_repo :: Maybe String } deriving (Show, Generic)
instance FromJSON Config
