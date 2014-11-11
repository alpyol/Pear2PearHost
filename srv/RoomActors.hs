{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module RoomActors (runRoomServer) where

import qualified Network.WebSockets as WS

import Control.Distributed.Process as DP
import Control.Distributed.Process.Node

import Data.Binary
import Data.Typeable
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.DeriveTH

import qualified Data.Aeson as AES ((.:), (.:?), decode, FromJSON(..), Value(..))
import Data.Aeson.Types

data SocketMessage = SocketMessage BS.ByteString deriving (Show, Typeable {-!, Binary !-})
$( derive makeBinary ''SocketMessage )

data RoomState = RoomState { getRoomURLs :: [String] }

initialRoomState :: RoomState 
initialRoomState = RoomState []

logMessage :: BS.ByteString -> Process RoomState
logMessage msg = do
    say $ "got ByteString: " ++ BS.unpack msg ++ "\r\n"
    return initialRoomState

processSocketMesssage :: SocketMessage -> Process RoomState
processSocketMesssage (SocketMessage msg) = do
    -- {"type":"imageAdded","url":"https://pp.vk.me/c624927/v624927433/8eaa/xxCjYjDRAxk.jpg"}
    let jsonValOpt = AES.decode msg :: Maybe Value
    case jsonValOpt of
        (Just (Object jsonVal)) -> do

            let cmdType :: Maybe String = parseMaybe (.: "type") jsonVal

            case cmdType of
                (Just "imageAdded") ->
                    say $ "TODO process imageAdded object: " ++ show jsonVal
                (Just someCmd) -> say $ "got unsupported command: " ++ show jsonVal
                Nothing -> say $ "no command in kson: " ++ show jsonVal

        (Just jsonVal) -> say $ "got unsupported json object: " ++ show jsonVal
        Nothing -> say $ "can not parse json object: " ++ show (BS.unpack msg)

    -- TODO implement this
    return initialRoomState

roomProcess :: RoomState -> Process ()
roomProcess state = do
    -- Test our matches in order against each message in the queue
    newState <- receiveWait [match logMessage, match processSocketMesssage]
    roomProcess newState

roomSocketProcess :: ProcessId -> WS.Connection -> Process ()
roomSocketProcess processId conn = do
    result <- liftIO $ WS.receive conn
    case result of
        (WS.ControlMessage (WS.Close code msg)) -> do
            say $ "did receiveData command with code: " ++ show code ++ " msg: " ++ BS.unpack msg
            -- TODO send die to roomProcess
            return ()
        (WS.DataMessage (WS.Text msg)) -> do
            send processId (SocketMessage msg)
            roomSocketProcess processId conn
        (WS.DataMessage (WS.Binary msg)) ->
            -- TODO send die to roomProcess
            return ()

roomApplication :: LocalNode -> WS.PendingConnection -> IO ()
roomApplication node pending = do
    conn <- WS.acceptRequest pending

    liftIO $ Prelude.putStrLn "got new connection"
    roomProcessID <- forkProcess node (roomProcess initialRoomState)
    runProcess node $ roomSocketProcess roomProcessID conn

    return ()

runRoomServer :: LocalNode -> IO ()
runRoomServer node = WS.runServer "127.0.0.1" 27001 $ roomApplication node
