module Control.Distributed.WebSocket.WebSocketProcess (forkWebSocketProcess) where

import qualified Network.WebSockets as WS

import Control.Distributed.Process as DP
import Control.Distributed.Process.Node

forkWebSocketProcess :: String -> Int -> LocalNode -> DP.ProcessId -> IO DP.ProcessId
forkWebSocketProcess = undefined
