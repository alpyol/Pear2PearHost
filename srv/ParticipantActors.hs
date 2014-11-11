module ParticipantActors (runParticipantServer) where

import qualified Network.WebSockets as WS

import Control.Distributed.Process as DP
import Control.Distributed.Process.Node

import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Data.Aeson as AES ((.:), (.:?), decode, FromJSON(..), Value(..))
import Data.Aeson.Types

import ActorsMessages (SocketMsg(..))

data ParticipantState = ParticipantState { getSupervisor :: DP.ProcessId }

initialParticipantState :: DP.ProcessId -> ParticipantState
initialParticipantState = ParticipantState

logMessage :: BS.ByteString -> Process (Maybe ParticipantState)
logMessage msg = do
    say $ "got unhandled string: " ++ BS.unpack msg ++ "\r\n"
    return Nothing

processSocketMesssage :: ParticipantState -> SocketMsg -> Process (Maybe ParticipantState)
processSocketMesssage state (SocketMsg msg) = do
    return Nothing
processSocketMesssage state (SocketMsg msg) = do
    die ("Socket closed - close participant" :: String)
    return Nothing

participantProcess :: ParticipantState -> Process ()
participantProcess state = do
    -- Test our matches in order against each message in the queue
    newState <- receiveWait [match (processSocketMesssage state), match logMessage]
    participantProcess $ maybe state id newState

participantSocketProcess :: ProcessId -> WS.Connection -> Process ()
participantSocketProcess processId conn = do
    result <- liftIO $ WS.receive conn
    case result of
        (WS.ControlMessage (WS.Close code msg)) -> do
            --say $ "did receiveData command with code: " ++ show code ++ " msg: " ++ BS.unpack msg
            send processId CloseMsg
            return ()
        (WS.DataMessage (WS.Text msg)) -> do
            send processId (SocketMsg msg)
            participantSocketProcess processId conn
        (WS.DataMessage (WS.Binary msg)) ->
            -- TODO send die to roomProcess
            return ()

participantApplication :: LocalNode -> DP.ProcessId -> WS.PendingConnection -> IO ()
participantApplication node supervisorProcessID pending = do
    conn <- WS.acceptRequest pending

    --liftIO $ Prelude.putStrLn "got new connection"
    participantProcessID <- forkProcess node (participantProcess $ initialParticipantState supervisorProcessID)
    runProcess node $ participantSocketProcess participantProcessID conn

    return ()

runParticipantServer :: LocalNode -> DP.ProcessId -> IO ()
runParticipantServer node supervisor = WS.runServer "127.0.0.1" 27002 $ participantApplication node supervisor
