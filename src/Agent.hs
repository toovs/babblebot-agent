{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures #-}

module Agent
  ( Agent(..),
    Action(..)
  ) where

import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Sequence as S (Seq)

data Agent = Agent { actions :: S.Seq Action, channelName :: String, token :: String } deriving (Generic)

data Action = SceneChange { sceneName :: String } deriving (Generic)

instance Serialize Agent
instance Serialize Action
instance FromJSON Agent
instance FromJSON Action
instance ToJSON Action
