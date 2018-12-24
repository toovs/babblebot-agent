{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Config(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

data Config = Config { channel :: String, host :: String, port :: Int, obs_port :: Int } deriving (Show, Generic)
instance FromJSON Config
