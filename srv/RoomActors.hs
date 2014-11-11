{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
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

import ActorsMessages (URLAddedMessage)

data SocketMessage = SocketMessage BS.ByteString deriving (Show, Typeable {-!, Binary !-})
$( derive makeBinary ''SocketMessage )

data RoomState = RoomState { getRoomURLs :: [String] }

initialRoomState :: RoomState 
initialRoomState = RoomState []

logMessage :: BS.ByteString -> Process (Maybe RoomState)
logMessage msg = do
    say $ "got unhandled string: " ++ BS.unpack msg ++ "\r\n"
    return Nothing

processAddImageCmd :: Object -> RoomState -> Process (Maybe RoomState)
processAddImageCmd json state = do

    -- {"type":"imageAdded","url":"https://pp.vk.me/c624927/v624927433/8eaa/xxCjYjDRAxk.jpg"}
    let addedImage :: Maybe String = parseMaybe (.: "url") json
    case addedImage of
        (Just addedImage) -> do
            say $ "TODO process imageAdded object: " ++ addedImage
            return Nothing
        Nothing -> do
            say $ "no image in json: " ++ show json
            return Nothing

jsonObjectWithType :: BS.ByteString -> Either String (String, Object)
jsonObjectWithType jsonStr = 
    case AES.decode jsonStr :: Maybe Value of
        (Just (Object json)) -> do
            let cmdType :: Maybe String = parseMaybe (.: "type") json
            case cmdType of
                (Just someCmd) -> Right (someCmd, json)
                Nothing -> Left $ "no command in json: " ++ show json
        (Just jsonVal) -> Left $ "got unsupported json object: " ++ show jsonVal
        Nothing -> Left $ "can not parse json object: " ++ show (BS.unpack jsonStr)

processSocketMesssage :: RoomState -> SocketMessage -> Process (Maybe RoomState)
processSocketMesssage state (SocketMessage msg) = do
    case jsonObjectWithType msg of
        (Right ("imageAdded", json)) -> do
            processAddImageCmd json state
        (Right (cmd, json)) -> do
            say $ "room got unsupported command: " ++ cmd ++ " json: " ++ show json
            return Nothing
        Left description -> do
            say description
            return Nothing

roomProcess :: RoomState -> Process ()
roomProcess state = do
    -- Test our matches in order against each message in the queue
    newState <- receiveWait [match (processSocketMesssage state), match logMessage]
    roomProcess $ maybe state id newState

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
