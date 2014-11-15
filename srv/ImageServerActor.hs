{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ImageServerActor (forkImageServer) where

import Control.Distributed.Process as DP
import Control.Distributed.Process.Node
import Control.Distributed.WebSocket.Process
import Control.Distributed.WebSocket.Types

import qualified Data.Aeson as AES (encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Aeson.Types
import Data.Maybe
import Data.Text

import ActorsMessages (ImgSrvToClientMsg(..), ClientToImgSrvMsg(..))

import ActorsCmn (jsonObjectWithType, withCpid)

import GHC.Conc.Sync

import WebMessagesData (Answer(..), ClientCandidate(..))

data ImageServerState = ImageServerState { webSocket :: DP.ProcessId }

initialImageServerState :: DP.ProcessId -> ImageServerState 
initialImageServerState = ImageServerState

logMessage :: BS.ByteString -> Process (Maybe ImageServerState)
logMessage msg = do
    say $ "got unhandled string: " ++ BS.unpack msg ++ "\r\n"
    return Nothing

processSendIceCandidateCmd :: Object -> ImageServerState -> Process (Maybe ImageServerState)
processSendIceCandidateCmd json state =
    -- {"msgType":"SendIceCandidate","cpid":"pid://127.0.0.1:10501:0:17","candidate":"..."}
    withCpid json Nothing $ \client -> do
        let candidateOpt :: Maybe String = parseMaybe (.: "candidate") json
        case candidateOpt of
            (Just candidate) -> do
                self <- getSelfPid
                send client $ Candidate self (BS.pack candidate)
                return Nothing
            Nothing -> do
                say $ "room: no candidate in json: " ++ show json
                return Nothing

processSendOfferCmd :: Object -> ImageServerState -> Process (Maybe ImageServerState)
processSendOfferCmd json state =
    -- {"msgType":"SendOffer","cpid":"pid://127.0.0.1:10501:0:17","offer":"..."}
    withCpid json Nothing $ \client -> do
        let offerOpt :: Maybe String = parseMaybe (.: "offer") json
        case offerOpt of
            (Just offer) -> do
                self <- getSelfPid
                send client $ Offer self (BS.pack offer)
                return Nothing
            Nothing -> do
                say $ "room: no offer in json: " ++ show json
                return Nothing

processSocketMesssage :: ImageServerState -> Receive -> Process (Maybe ImageServerState)
processSocketMesssage state (Text msg) =
    case jsonObjectWithType msg of
        (Right ("SendIceCandidate", json)) -> processSendIceCandidateCmd json state
        (Right ("SendOffer"       , json)) -> processSendOfferCmd        json state
        (Right (cmd, json)) -> do
            say $ "imageSrv: got unsupported command: " ++ cmd ++ " json: " ++ show json
            return Nothing
        Left description -> do
            say description
            return Nothing
processSocketMesssage state (Closed _ _) = do
    self <- getSelfPid
    die ("Socket closed - close room" :: String)
    return Nothing

--data ClientToImgSrvMsg = SendAnswer BS.ByteString
processClientMesssage :: ImageServerState -> ClientToImgSrvMsg -> Process (Maybe ImageServerState)
processClientMesssage state (SendAnswer answer) = do
    send (webSocket state) $ SendTextData $ AES.encode $ Answer (pack $ BS.unpack answer)
    return Nothing
processClientMesssage state (SendRemoteIceCandidate candidate) = do
    send (webSocket state) $ SendTextData $ AES.encode $ ClientCandidate (pack $ BS.unpack candidate)
    return Nothing

imageSrvProcess' :: ImageServerState -> Process ()
imageSrvProcess' state = do
    -- Test our matches in order against each message in the queue
    newState <- receiveWait [
        match (processSocketMesssage state),
        match (processClientMesssage state),
        match logMessage ]
    imageSrvProcess' $ fromMaybe state newState

imageSrvProcess :: DP.ProcessId -> Process ()
imageSrvProcess socket = imageSrvProcess' $ initialImageServerState socket

forkImageServer :: LocalNode -> IO ThreadId
forkImageServer node = forkWebSocketProcess "127.0.0.1" 27003 node imageSrvProcess
