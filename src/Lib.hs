{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( babblebotAgent
  ) where

import System.IO (hClose, hGetLine, hPutStrLn, hSetBuffering, BufferMode(..))
import System.Directory (renameFile, removeFile, getCurrentDirectory, doesFileExist)
import System.Exit
import System.Info
import System.Process (runCommand)
import Data.Monoid ((<>))
import Data.Serialize
import Control.Lens ((^.), (.~), (&), (%~))
import Control.Concurrent
import Network
import Network.Socket (withSocketsDo)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Network.Wreq as W
import qualified Network.WebSockets as WS
import qualified Data.Yaml as Y (ParseException, decodeFileEither)
import qualified Data.ByteString.Lazy as LB (writeFile)
import qualified Data.ByteString.Char8 as C (pack, unpack)
import qualified Data.Sequence as S (empty, singleton)
import qualified Control.Exception as E

import Agent
import Config

agentVersion = "v0.1.2.0"

babblebotAgent = do
  dir <- getCurrentDirectory
  case System.Info.os of
    "mingw32" -> do
      exists <- doesFileExist (dir ++ "\\.babblebot-agent.exe.old")
      if exists then removeFile (dir ++ "\\.babblebot-agent.exe.old") else pure ()
    "linux" -> do
      pure ()
    _ -> pure ()
  parsedContent <- Y.decodeFileEither "config.yaml" :: IO (Either Y.ParseException Config)
  case parsedContent of
    Left e -> error (show e)
    Right config -> poll config-- `E.catch` (eHandler config)
  where
    poll config@(Config channel secret host port obsPort updateRepo) = do
      withSocketsDo $ do
        handle <- connectTo host (PortNumber (fromIntegral port))
        hSetBuffering handle LineBuffering
        let update = Update { version = agentVersion }
            authAgent = Agent { actions = S.singleton update, channelName = channel, token = secret }
            authEncoded = encode authAgent
        hPutStrLn handle (C.unpack authEncoded)
        contents <- hGetLine handle
        let Right agent = decode $ C.pack contents
        mapM_ (runAction updateRepo obsPort) (actions agent)
        hClose handle
      threadDelay 10000000
      poll config
    runAction updateRepo obsPort action = do
      case action of
        Update ver -> do
          case updateRepo of
            Nothing -> pure ()
            Just repo -> do
              let url = "https://github.com/" ++ repo ++ "/releases/download/" ++ agentVersion ++ "/babblebot-agent.exe"
              rsp <- W.get url
              let code = rsp ^. W.responseStatus . W.statusCode
                  body = rsp ^. W.responseBody
              case code of
                200 -> do
                  dir <- getCurrentDirectory
                  case System.Info.os of
                    "mingw32" -> do
                      renameFile (dir ++ "\\babblebot-agent.exe") (dir ++ "\\.babblebot-agent.exe.old")
                      LB.writeFile (dir ++ "\\babblebot-agent.exe") body
                      runCommand ("cmd /K \"" ++ dir ++ "\\babblebot-agent.exe\"")
                      pure ()
                    "linux" -> do
                      pure ()
                    _ -> pure ()
                  exitSuccess
                _ -> pure ()
        SceneChange name -> do
          withSocketsDo $ WS.runClient "localhost" obsPort "/" $ \conn -> do
            let json = "{\"request-type\":\"SetCurrentScene\",\"scene-name\":\"" <> T.pack name <> "\",\"message-id\":0}"
            WS.sendTextData conn json
            WS.sendClose conn ("" :: Text)
    eHandler :: Config -> E.IOException -> IO ()
    eHandler config e = do
      threadDelay 10000000
      poll config `E.catch` (eHandler config)
