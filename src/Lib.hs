{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( babblebotAgent
  ) where

import System.IO (hClose, hReady, hGetLine, hPutStrLn, hSetBuffering, stdout, BufferMode(..))
import System.Directory (renameFile, removeFile, getCurrentDirectory, doesFileExist)
import System.Exit
import System.Info
import System.Process (runCommand)
import System.Win32.Automation.Input (sendInput, makeKeyboardInput)
import Data.Word (Word32)
import Data.Monoid ((<>))
import Data.Serialize
import Control.Lens ((^.), (.~), (&), (%~))
import Control.Concurrent
import Control.Concurrent.Async
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

agentVersion = "v0.2.3.0"

babblebotAgent = do
  hSetBuffering stdout LineBuffering
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
    Right config -> poll config `E.catch` (eHandler config)
  where
    poll config@(Config channel secret host port obsHost obsPort updateRepo) = do
      withSocketsDo $ do
        handle <- connectTo host (PortNumber (fromIntegral port))
        putStrLn "connected to master babblebot process"
        hSetBuffering handle LineBuffering
        let update = Update { version = agentVersion }
            authAgent = Agent { actions = S.singleton update, channelName = channel, token = secret }
            authEncoded = encode authAgent
        hPutStrLn handle (C.unpack authEncoded)
        contents <- hWaitForLine handle 4
        case contents of
          Left _ -> hClose handle
          Right agent' ->
            case decode $ C.pack agent' of
              Left err -> hClose handle
              Right agent -> do
                mapM_ (runAction updateRepo obsHost obsPort) (actions agent)
                hClose handle
      threadDelay 10000000
      poll config `E.catch` (eHandler config)
    hWaitForLine handle secs = race (threadDelay (secs * 1000000)) (hGetLine handle)
    runAction updateRepo obsHost obsPort action = do
      case action of
        Update ver -> do
          putStrLn "received update action"
          case updateRepo of
            Nothing -> pure ()
            Just repo -> do
              let url = "https://github.com/" ++ repo ++ "/releases/download/" ++ ver ++ "/babblebot-agent.exe"
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
        Input keys -> do
          putStrLn "received input action"
          inputP <- sequence $ map (\key -> makeKeyboardInput key Nothing) keys
          sendInput inputP
          inputR <- sequence $ map (\key -> makeKeyboardInput key (Just (2::Word32))) keys
          sendInput inputR
          pure ()
        SceneChange name -> do
          putStrLn "received scene-change action"
          withSocketsDo $ WS.runClient obsHost obsPort "/" $ \conn -> do
            let json = "{\"request-type\":\"SetCurrentScene\",\"scene-name\":\"" <> T.pack name <> "\",\"message-id\":0}"
            WS.sendTextData conn json
            WS.sendClose conn ("" :: Text)
    eHandler :: Config -> E.IOException -> IO ()
    eHandler config e = do
      print e
      threadDelay 10000000
      poll config `E.catch` (eHandler config)
