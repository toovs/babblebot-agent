{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures #-}

module Agent
  ( Agent(..),
    Action(..)
  ) where

import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Data.Aeson (ToJSON)
import qualified Data.Sequence as S (Seq)

data Agent = Agent { actions :: S.Seq Action } deriving (Generic)

data Action = SceneChange { sceneName :: String } deriving (Generic)

instance Serialize Agent
instance Serialize Action
instance ToJSON Action
