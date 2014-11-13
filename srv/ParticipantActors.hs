{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module ParticipantActors (runParticipantServer) where

import qualified Network.WebSockets as WS

import Control.Distributed.Process as DP
import Control.Distributed.Process.Node

import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Aeson.Types
import Data.Text
import Data.Maybe

import ActorsMessages (
    SocketMsg(..),
    FromClientMsg(..),
    SupervisorToClientMsg(..),
    RoomToClientMsg(..))

import ActorsCmn (jsonObjectWithType)

data ParticipantState = ParticipantState { getSupervisor :: DP.ProcessId, getConnection :: WS.Connection }

initialParticipantState :: DP.ProcessId -> WS.Connection -> ParticipantState
initialParticipantState = ParticipantState

logMessage :: BS.ByteString -> Process (Maybe ParticipantState)
logMessage msg = do
    say $ "got unhandled string: " ++ BS.unpack msg ++ "\r\n"
    return Nothing

processOfferCmd :: Object -> ParticipantState -> Process (Maybe ParticipantState)
processOfferCmd json state = do
    -- {"type":"RequestOffer","url":"https://pp.vk.me/c624927/v624927433/8eaa/xxCjYjDRAxk.jpg"}
    let orlOpt :: Maybe String = parseMaybe (.: "url") json
    case orlOpt of
        (Just url) -> do
            self <- getSelfPid
            send (getSupervisor state) (RequestOffer self (BS.pack url))
            return Nothing
        Nothing -> do
            say $ "client: no image url in json: " ++ show json
            return Nothing

processSocketMesssage :: ParticipantState -> SocketMsg -> Process (Maybe ParticipantState)
processSocketMesssage state (SocketMsg msg) = do
    -- jsonObjectWithType :: BS.ByteString -> Either String (String, Object)
    case jsonObjectWithType msg of
        (Right ("RequestOffer", json)) ->
            processOfferCmd json state
        (Right (cmd, json)) -> do
            say $ "client: got unsupported command: " ++ cmd ++ " json: " ++ show json
            return Nothing
        Left description -> do
            say description
            return Nothing
    return Nothing
processSocketMesssage state CloseMsg = do
    die ("Socket closed - close participant" :: String)
    return Nothing

processSupervisorCmds :: ParticipantState -> SupervisorToClientMsg -> Process (Maybe ParticipantState)
processSupervisorCmds state NoImageError = do
    liftIO $
        let conn = getConnection state
        in do
            WS.sendTextData conn ("{\"msgType\":\"NoRequestedURL\"}" :: Text)
            WS.sendClose conn ("no url" :: Text)
    return Nothing

--TODO process - NoImageOnWebError
--RoomToClientMsg
processRoomCmds :: ParticipantState -> RoomToClientMsg -> Process (Maybe ParticipantState)
processRoomCmds state NoImageOnWebError = do
    say $ "client: got: NoImageOnWebError error TODO send to client socket"
    return Nothing

participantProcess :: ParticipantState -> Process ()
participantProcess state = do
    -- Test our matches in order against each message in the queue
    newState <- receiveWait [
        match (processSocketMesssage state),
        match (processSupervisorCmds state),
        match (processRoomCmds       state),
        match logMessage ]
    participantProcess $ fromMaybe state newState

participantSocketProcess :: ProcessId -> WS.Connection -> Process ()
participantSocketProcess processId conn = do
    result <- liftIO $ WS.receive conn
    case result of
        (WS.ControlMessage (WS.Close code msg)) -> do
            --say $ "did receiveData command with code: " ++ show code ++ " msg: " ++ BS.unpack msg
            send processId CloseMsg
            return ()
        (WS.DataMessage (WS.Text msg)) -> do
            say $ "client: did receiveData msg: " ++ BS.unpack msg
            send processId (SocketMsg msg)
            participantSocketProcess processId conn
        (WS.DataMessage (WS.Binary msg)) ->
            -- TODO send die to roomProcess
            return ()

participantApplication :: LocalNode -> DP.ProcessId -> WS.PendingConnection -> IO ()
participantApplication node supervisorProcessID pending = do
    conn <- WS.acceptRequest pending

    liftIO $ Prelude.putStrLn "client: got new client connection"
    participantProcessID <- forkProcess node (participantProcess $ initialParticipantState supervisorProcessID conn)
    runProcess node $ participantSocketProcess participantProcessID conn

    return ()

runParticipantServer :: LocalNode -> DP.ProcessId -> IO ()
runParticipantServer node supervisor = WS.runServer "127.0.0.1" 27002 $ participantApplication node supervisor
