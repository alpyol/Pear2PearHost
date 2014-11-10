{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveDataTypeable    #-}

module Actors (createAndInitActors) where

import Data.Text (Text, pack, unpack)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

import Control.Distributed.Process as DP
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable
import Network.Transport.TCP

import Data.Binary
import Data.Typeable
import qualified Data.ByteString.Lazy.Char8 as BS

roomProcess :: Process ()
roomProcess = do

    -- liftIO $ Prelude.putStrLn "in room"

    --self <- getSelfPid
    --send self ("hello" :: BS.ByteString)
    hello <- expect :: Process String
    liftIO $ Prelude.putStrLn hello
    roomProcess

roomSocketProcess :: WS.Connection -> Process ()
roomSocketProcess conn = do
    result <- liftIO $ WS.receive conn
    case result of
        (WS.ControlMessage msg) -> do
            liftIO $ Prelude.putStrLn "did receiveData command: "
            return (BS.pack "got cmd")
        (WS.DataMessage (WS.Text msg)) -> do
            liftIO $ Prelude.putStrLn ("did receiveData Msg 1: " ++ BS.unpack msg)
            return msg
        (WS.DataMessage (WS.Binary msg)) -> do
            liftIO $ Prelude.putStrLn ("did receiveData Msg 2: " ++ BS.unpack msg)
            return (BS.pack "got data")
    roomSocketProcess conn

roomApplication :: LocalNode -> WS.PendingConnection -> IO ()
roomApplication node pending = do
    conn <- WS.acceptRequest pending

    liftIO $ Prelude.putStrLn "got new connection"
    roomProcessID <- forkProcess node roomProcess
    runProcess node $ roomSocketProcess conn

    return ()

createAndInitActors = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable

    forkIO $ WS.runServer "127.0.0.1" 27001 $ roomApplication node

    return ()
