{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( babblebotAgent
  ) where

import System.IO (hClose, hGetLine, hPutStrLn, hSetBuffering, BufferMode(..))
import Data.Monoid ((<>))
import Data.Serialize
import Control.Concurrent
import Network
import Network.Socket (withSocketsDo)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import qualified Data.Yaml as Y (ParseException, decodeFileEither)
import qualified Data.ByteString.Char8 as C (pack, unpack)
import qualified Data.Sequence as S (empty)
import qualified Control.Exception as E

import Agent
import Config

babblebotAgent = do
  parsedContent <- Y.decodeFileEither "config.yaml" :: IO (Either Y.ParseException Config)
  case parsedContent of
    Left e -> error (show e)
    Right config -> poll config `E.catch` (eHandler config)
  where
    poll config@(Config channel secret host port obsPort) = do
      withSocketsDo $ do
        handle <- connectTo host (PortNumber (fromIntegral port))
        hSetBuffering handle LineBuffering
        let authAgent = Agent { actions = S.empty, channelName = channel, token = secret }
            authEncoded = encode authAgent
        hPutStrLn handle (C.unpack authEncoded)
        contents <- hGetLine handle
        let Right agent = decode $ C.pack contents
        mapM_ (runAction obsPort) (actions agent)
        hClose handle
      threadDelay 10000000
      poll config
    runAction obsPort action = do
      case action of
        SceneChange name -> do
          withSocketsDo $ WS.runClient "localhost" obsPort "/" $ \conn -> do
            let json = "{\"request-type\":\"SetCurrentScene\",\"scene-name\":\"" <> T.pack name <> "\",\"message-id\":0}"
            WS.sendTextData conn json
            WS.sendClose conn ("" :: Text)
    eHandler :: Config -> E.IOException -> IO ()
    eHandler config e = do
      threadDelay 10000000
      poll config `E.catch` (eHandler config)
