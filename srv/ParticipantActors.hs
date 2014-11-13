{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module ParticipantActors (runParticipantServer) where

import qualified Network.WebSockets as WS

import Control.Distributed.Process as DP
import Control.Distributed.Process.Node

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Aeson as AES (encode)

import Data.Aeson.Types
import Data.Text
import Data.Maybe
import Control.Applicative ((<$>))

import ActorsMessages (
    SocketMsg(..),
    SupervisorToClientMsg(..),
    RoomToClientMsg(..),
    ClientToSupervisorMsg(..),
    ClientToRoomMsg(..))

import WebMessagesData

import ActorsCmn (jsonObjectWithType)

data ParticipantState = ParticipantState {
    getSupervisor :: DP.ProcessId ,
    getConnection :: WS.Connection,
    getURL        :: Maybe BS.ByteString,
    getRoom       :: Maybe DP.ProcessId }

initialParticipantState :: DP.ProcessId -> WS.Connection -> ParticipantState
initialParticipantState supervisor conn = ParticipantState supervisor conn Nothing Nothing

putRoomToState :: ParticipantState -> DP.ProcessId -> ParticipantState
putRoomToState state room = ParticipantState (getSupervisor state) (getConnection state) (getURL state) (Just room)

putURLToState :: ParticipantState -> BS.ByteString -> ParticipantState
putURLToState state url = ParticipantState (getSupervisor state) (getConnection state) (Just url) (getRoom state)

logMessage :: BS.ByteString -> Process (Maybe ParticipantState)
logMessage msg = do
    say $ "got unhandled string: " ++ BS.unpack msg ++ "\r\n"
    return Nothing

processOfferCmd :: Object -> ParticipantState -> Process (Maybe ParticipantState)
processOfferCmd json state = do
    -- {"type":"RequestOffer","url":"https://pp.vk.me/c624927/v624927433/8eaa/xxCjYjDRAxk.jpg"}
    let orlOpt :: Maybe BS.ByteString = BS.pack <$> parseMaybe (.: "url") json
    case orlOpt of
        (Just url) -> do
            self <- getSelfPid
            send (getSupervisor state) (GetRoom self url)
            return $ Just $ putURLToState state url
        Nothing -> do
            say $ "client: no image url in json: " ++ show json
            return Nothing

processSocketMesssage :: ParticipantState -> SocketMsg -> Process (Maybe ParticipantState)
processSocketMesssage state (SocketMsg msg) = do
    -- jsonObjectWithType :: BS.ByteString -> Either String (String, Object)
    case jsonObjectWithType msg of
        (Right ("RequestOffer", json)) -> processOfferCmd json state
        (Right (cmd, json)) -> do
            say $ "client: got unsupported command: " ++ cmd ++ " json: " ++ show json
            return Nothing
        Left description -> do
            say description
            return Nothing
processSocketMesssage state CloseMsg = do
    die ("Socket closed - close participant" :: String)
    return Nothing

sendToWebNoURL :: WS.Connection -> IO ()
sendToWebNoURL conn = do
    WS.sendTextData conn ("{\"msgType\":\"NoRequestedURL\"}" :: Text)
    WS.sendClose conn ("no url" :: Text)

sendInvalidParticipantState :: WS.Connection -> Text -> IO ()
sendInvalidParticipantState conn text = do
    WS.sendTextData conn $ AES.encode $ ClientError text
    WS.sendClose conn ("internal state error" :: Text)

processSupervisorCmds :: ParticipantState -> SupervisorToClientMsg -> Process (Maybe ParticipantState)
processSupervisorCmds state (URLRoom room) = do
    case (getURL state) of
        (Just url) -> do
            self <- getSelfPid
            send room (RequestOffer self url)
            return $ Just $ putRoomToState state room
        Nothing -> do
            say $ "client: no url in state: fix me"
            liftIO $ sendInvalidParticipantState (getConnection state) "client internal state error: no url in state: fix me"
            return Nothing
processSupervisorCmds state NoImageError = do
    liftIO $ sendToWebNoURL $ getConnection state
    return Nothing

sendToWebOffer :: WS.Connection -> Text -> IO ()
sendToWebOffer conn offer = do
    WS.sendTextData conn $ json where
        json = AES.encode $ ClientOffer offer

sendToWebCandidate :: WS.Connection -> Text -> IO ()
sendToWebCandidate conn candidate = do
    WS.sendTextData conn $ json where
        json = AES.encode $ ClientCandidate candidate

processRoomCmds :: ParticipantState -> RoomToClientMsg -> Process (Maybe ParticipantState)
processRoomCmds state NoImageOnWebError = do
    liftIO $ sendToWebNoURL $ getConnection state
    return Nothing
processRoomCmds state (Offer offer) = do
    liftIO $ sendToWebOffer (getConnection state) (pack $ BS.unpack offer)
    return Nothing
processRoomCmds state (Candidate candidate) = do
    liftIO $ sendToWebCandidate (getConnection state) (pack $ BS.unpack candidate)
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
