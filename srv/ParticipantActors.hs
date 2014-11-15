{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module ParticipantActors (forkParticipantServer) where

import Control.Distributed.Process as DP
import Control.Distributed.Process.Node
import Control.Distributed.WebSocket.Process
import Control.Distributed.WebSocket.Types

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Aeson as AES (encode)

import Data.Aeson.Types
import Data.Text
import Data.Maybe
import Control.Applicative ((<$>))

import GHC.Conc.Sync

import ActorsMessages (
    SupervisorToClientMsg(..),
    ClientToSupervisorMsg(..),
    ClientToRoomMsg(..),
    ImgSrvToClientMsg(..),
    ClientToImgSrvMsg(..))

import WebMessagesData (
    ClientError(..),
    ClientOffer(..),
    ClientCandidate(..),
    ClientNoImageError(..))

import ActorsCmn (jsonObjectWithType)

data ParticipantState = ParticipantState {
    supervisor :: DP.ProcessId,
    webSocket  :: DP.ProcessId,
    getURL     :: Maybe BS.ByteString,
    imgSrv     :: Maybe DP.ProcessId }

initialParticipantState :: DP.ProcessId -> DP.ProcessId -> ParticipantState
initialParticipantState supervisor socket = ParticipantState supervisor socket Nothing Nothing

putImgSrvToState :: ParticipantState -> DP.ProcessId -> ParticipantState
putImgSrvToState state imgSrv = ParticipantState (supervisor state) (webSocket state) (getURL state) (Just imgSrv)

putURLToState :: ParticipantState -> BS.ByteString -> ParticipantState
putURLToState state url = ParticipantState (supervisor state) (webSocket state) (Just url) (imgSrv state)

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
            send (supervisor state) (GetRoom self url)
            return $ Just $ putURLToState state url
        Nothing -> do
            say $ "client: no image url in json: " ++ show json
            return Nothing

withImgSrv :: ParticipantState -> (DP.ProcessId -> Process (Maybe ParticipantState)) -> Process (Maybe ParticipantState)
withImgSrv state handler =
    case imgSrv state of
        (Just imgSrv) -> handler imgSrv
        Nothing -> do
            say "client: invalid internal state, no imgSrv: fix me"
            sendInvalidParticipantState (webSocket state) "client: invalid internal state, no imgSrv: fix me"
            return Nothing

processSendAnswerCmd :: Object -> ParticipantState -> Process (Maybe ParticipantState)
processSendAnswerCmd json state = do
    -- {"type":"SendAnswer","answer":"..."}
    let answerOpt :: Maybe BS.ByteString = BS.pack <$> parseMaybe (.: "answer") json
    case answerOpt of
        (Just answer) ->
            withImgSrv state $ \imgSrv -> do
                send imgSrv (SendAnswer answer)
                return Nothing
        Nothing -> do
            say $ "client: no image url in json: " ++ show json
            return Nothing

processIceCandidateCmd :: Object -> ParticipantState -> Process (Maybe ParticipantState)
processIceCandidateCmd json state = do
    -- {"type":"SendAnswer","answer":"..."}
    let candidateOpt :: Maybe BS.ByteString = BS.pack <$> parseMaybe (.: "candidate") json
    case candidateOpt of
        (Just candidate) ->
            withImgSrv state $ \imgSrv -> do
                send imgSrv (SendRemoteIceCandidate candidate)
                return Nothing
        Nothing -> do
            say $ "client: no candidate in json: " ++ show json
            return Nothing

processSocketMesssage :: ParticipantState -> Receive -> Process (Maybe ParticipantState)
processSocketMesssage state (Text msg) =
    -- jsonObjectWithType :: BS.ByteString -> Either String (String, Object)
    case jsonObjectWithType msg of
        (Right ("RequestOffer"          , json)) -> processOfferCmd        json state
        (Right ("SendAnswer"            , json)) -> processSendAnswerCmd   json state
        (Right ("SendRemoteIceCandidate", json)) -> processIceCandidateCmd json state
        (Right (cmd, json)) -> do
            say $ "client: got unsupported command: " ++ cmd ++ " json: " ++ show json
            return Nothing
        Left description -> do
            say $ "room: " ++ description
            return Nothing
processSocketMesssage state (Closed _ _) = do
    die ("Socket closed - close participant" :: String)
    return Nothing

sendToWebNoURL :: DP.ProcessId -> Process ()
sendToWebNoURL socket = do
    send socket (SendTextData $ AES.encode ClientNoImageError)
    send socket (Close "no url")

sendInvalidParticipantState :: DP.ProcessId -> Text -> Process ()
sendInvalidParticipantState socket text = do
    send socket (SendTextData $ AES.encode $ ClientError text)
    send socket (Close "internal state error")

processSupervisorCmds :: ParticipantState -> SupervisorToClientMsg -> Process (Maybe ParticipantState)
processSupervisorCmds state (URLRoom room) =
    case getURL state of
        (Just url) -> do
            self <- getSelfPid
            send room (RequestOffer self url)
            return Nothing
        Nothing -> do
            say "client: no url in state: fix me"
            sendInvalidParticipantState (webSocket state) "client internal state error: no url in state: fix me"
            return Nothing
processSupervisorCmds state NoImageError = do
    sendToWebNoURL $ webSocket state
    return Nothing

sendToWebOffer :: DP.ProcessId -> Text -> Process ()
sendToWebOffer socket offer =
    send socket $ SendTextData json where
        json = AES.encode $ ClientOffer offer

sendToWebCandidate :: DP.ProcessId -> Text -> Process ()
sendToWebCandidate socket candidate =
    send socket $ SendTextData json where
        json = AES.encode $ ClientCandidate candidate

processRoomCmds :: ParticipantState -> ImgSrvToClientMsg -> Process (Maybe ParticipantState)
processRoomCmds state (Offer imageSrv offer) = do
    sendToWebOffer (webSocket state) (pack $ BS.unpack offer)
    return $ Just $ putImgSrvToState state imageSrv
processRoomCmds state (Candidate imageSrv candidate) = do
    sendToWebCandidate (webSocket state) (pack $ BS.unpack candidate)
    return $ Just $ putImgSrvToState state imageSrv

participantProcess' :: ParticipantState -> Process ()
participantProcess' state = do
    -- Test our matches in order against each message in the queue
    newState <- receiveWait [
        match (processSocketMesssage state),
        match (processSupervisorCmds state),
        match (processRoomCmds       state),
        match logMessage ]
    participantProcess' $ fromMaybe state newState

participantProcess :: DP.ProcessId -> DP.ProcessId -> Process ()
participantProcess supervisor socket = participantProcess' $ initialParticipantState supervisor socket

forkParticipantServer :: LocalNode -> DP.ProcessId -> IO ThreadId
forkParticipantServer node supervisor = forkWebSocketProcess "127.0.0.1" 27002 node (participantProcess supervisor)
