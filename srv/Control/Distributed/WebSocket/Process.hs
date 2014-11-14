module Control.Distributed.WebSocket.Process (forkWebSocketProcess) where

import qualified Network.WebSockets as WS

import Control.Concurrent (forkIO)
import Control.Distributed.Process as DP
import Control.Distributed.Process.Node

import Control.Distributed.WebSocket.Types

import Data.Word
import GHC.Conc.Sync

processSend :: WS.Connection -> Send -> Process ()
processSend conn (SendTextData txt) = liftIO $ WS.sendTextData conn txt

webSocketSendProcess :: WS.Connection -> Process ()
webSocketSendProcess conn = do
    receiveWait [ match (processSend conn), match (processSend conn) ]
    webSocketSendProcess conn

webSocketReceiveProcess :: ProcessId -> WS.Connection -> Process ()
webSocketReceiveProcess handler conn = do
    result <- liftIO $ WS.receive conn
    case result of
        (WS.ControlMessage (WS.Close code msg)) -> do
            send handler (Closed code msg)
            return ()
        (WS.DataMessage (WS.Text msg)) -> do
            send handler (Text msg)
            webSocketReceiveProcess handler conn
        (WS.DataMessage (WS.Binary msg)) -> do
            send handler (Binary msg)
            webSocketReceiveProcess handler conn

webSocketApplication :: LocalNode -> (DP.ProcessId -> Process ()) -> WS.PendingConnection -> IO ()
webSocketApplication node acceptor pending = do
    conn <- WS.acceptRequest pending

    pid <- forkProcess node $ webSocketSendProcess conn

    handler <- forkProcess node $ acceptor pid

    runProcess node $ webSocketReceiveProcess handler conn
    return ()

forkWebSocketProcess :: String -> Int -> LocalNode -> (DP.ProcessId -> Process ()) -> IO (ThreadId)
forkWebSocketProcess host port node acceptor = do
    forkIO $ WS.runServer host port $ webSocketApplication node acceptor
