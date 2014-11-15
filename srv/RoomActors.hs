{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RoomActors (forkRoomServer) where

import Control.Distributed.Process as DP
import Control.Distributed.Process.Node
import Control.Distributed.WebSocket.Process
import Control.Distributed.WebSocket.Types

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Aeson as AES (encode)
import Data.Aeson.Types
import Data.Text
import Data.Maybe

import GHC.Conc.Sync

import ActorsMessages (
    FromRoomMsg(..),
    ClientToRoomMsg(..),
    RoomToClientMsg(..))

import qualified WebMessagesData as WD (RequestOffer(..))

import ActorsCmn (jsonObjectWithType, withCpid, pid2Str)

data RoomState = RoomState { supervisor :: DP.ProcessId, webSocket :: DP.ProcessId }

initialRoomState :: DP.ProcessId -> DP.ProcessId -> RoomState 
initialRoomState = RoomState

putWebSocket :: RoomState -> DP.ProcessId -> RoomState 
putWebSocket state = RoomState $ supervisor state

logMessage :: BS.ByteString -> Process (Maybe RoomState)
logMessage msg = do
    say $ "got unhandled string: " ++ BS.unpack msg ++ "\r\n"
    return Nothing

processAddImageCmd :: Object -> RoomState -> Process (Maybe RoomState)
processAddImageCmd json state = do
    -- {"msgType":"ImageAdded","url":"https://pp.vk.me/c624927/v624927433/8eaa/xxCjYjDRAxk.jpg"}
    let addedImage :: Maybe String = parseMaybe (.: "url") json
    case addedImage of
        (Just addedImage) -> do
            self <- getSelfPid
            send (supervisor state) (URLAddedMsg self (BS.pack addedImage))
            return Nothing
        Nothing -> do
            say $ "no image in json: " ++ show json
            return Nothing

processNoImageCmd :: Object -> RoomState -> Process (Maybe RoomState)
processNoImageCmd json state =
    -- {"msgType":"NoRequestedURL","cpid":"pid://127.0.0.1:10501:0:17"}
    withCpid json Nothing $ \client -> do
        send client NoImageOnWebError
        return Nothing

processSocketMesssage :: RoomState -> Receive -> Process (Maybe RoomState)
processSocketMesssage state (Text msg) =
    case jsonObjectWithType msg of
        (Right ("ImageAdded"    , json)) -> processAddImageCmd json state
        (Right ("NoRequestedURL", json)) -> processNoImageCmd  json state
        (Right (cmd, json)) -> do
            say $ "room: got unsupported command: " ++ cmd ++ " json: " ++ show json
            return Nothing
        Left description -> do
            say $ "room: " ++ description
            return Nothing
processSocketMesssage state (Closed _ _) = do
    self <- getSelfPid
    send (supervisor state) (RoomClosedMsg self)
    die ("Socket closed - close room" :: String)
    return Nothing

processClientMsgs :: RoomState -> ClientToRoomMsg -> Process (Maybe RoomState)
processClientMsgs state (RequestOffer client url) = do
    let cmd = AES.encode $ WD.RequestOffer (pack $ BS.unpack url) (pack $ pid2Str client)
        in do
            -- TODO handle exception on send here ???
            say $ "room: send to socket: " ++ BS.unpack cmd
            send (webSocket state) (SendTextData cmd)
    return Nothing

roomProcess' :: RoomState -> Process ()
roomProcess' state = do
    -- Test our matches in order against each message in the queue
    newState <- receiveWait [
        match (processSocketMesssage state),
        match (processClientMsgs     state),
        match logMessage ]
    roomProcess' $ fromMaybe state newState

roomProcess :: DP.ProcessId -> DP.ProcessId -> Process ()
roomProcess supervisor socket = roomProcess' $ initialRoomState supervisor socket

forkRoomServer :: LocalNode -> DP.ProcessId -> IO ThreadId
forkRoomServer node supervisor = forkWebSocketProcess "127.0.0.1" 27001 node (roomProcess supervisor)
