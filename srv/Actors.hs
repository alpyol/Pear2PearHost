{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Actors (createAndInitActors) where

--import Data.Text hiding (count)
--import Data.Char (isPunctuation, isSpace)
--import Data.Monoid (mappend)
import Data.Text (Text, pack, unpack)
--import Control.Exception (finally)
import Control.Monad (forM_, forever)
--import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

import Control.Concurrent
import Control.Distributed.Process as DP
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable
import Network.Transport.TCP

import Data.Binary
import Data.Typeable
--import qualified Data.ByteString as BS
--import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BS
--import Data.ByteString.Lazy.Char8

roomProcess :: WS.Connection -> Process ()
roomProcess conn = do

    liftIO $ Prelude.putStrLn "in room"

    msg <- liftIO $ do
        liftIO $ Prelude.putStrLn "try receive data"
        result <- WS.receive conn
        result1 <- case result of
            (WS.ControlMessage msg) -> do
                Prelude.putStrLn ("did receiveData command: ")
                return (BS.pack "got cmd")
            (WS.DataMessage (WS.Text msg)) -> do
                Prelude.putStrLn ("did receiveData Msg: " ++ (BS.unpack msg))
                return msg
            (WS.DataMessage (WS.Binary msg)) -> do
                Prelude.putStrLn ("did receiveData Msg: " ++ (BS.unpack msg))
                return (BS.pack "got data")
        return result1

    liftIO $ Prelude.putStrLn ("got msg: " ++ (BS.unpack msg))

    self <- getSelfPid
    send self ("hello" :: BS.ByteString)
    hello <- expect :: Process String
    liftIO $ Prelude.putStrLn hello
    return ()

roomApplication node pending = do
    conn <- WS.acceptRequest pending
    liftIO $ Prelude.putStrLn "got new connection"
    --forkProcess node (roomProcess conn)
    msg <- liftIO $ do
        liftIO $ Prelude.putStrLn "try receive data"
        result <- WS.receive conn
        result1 <- case result of
            (WS.ControlMessage msg) -> do
                Prelude.putStrLn ("did receiveData command: ")
                return (BS.pack "got cmd")
            (WS.DataMessage (WS.Text msg)) -> do
                Prelude.putStrLn ("did receiveData Msg: " ++ (BS.unpack msg))
                return msg
            (WS.DataMessage (WS.Binary msg)) -> do
                Prelude.putStrLn ("did receiveData Msg: " ++ (BS.unpack msg))
                return (BS.pack "got data")
        return result1

    msg <- liftIO $ do
        liftIO $ Prelude.putStrLn "try receive data"
        result <- WS.receive conn
        result1 <- case result of
            (WS.ControlMessage msg) -> do
                Prelude.putStrLn ("did receiveData command: ")
                return (BS.pack "got cmd")
            (WS.DataMessage (WS.Text msg)) -> do
                Prelude.putStrLn ("did receiveData Msg: " ++ (BS.unpack msg))
                return msg
            (WS.DataMessage (WS.Binary msg)) -> do
                Prelude.putStrLn ("did receiveData Msg: " ++ (BS.unpack msg))
                return (BS.pack "got data")
        return result1

    liftIO $ Prelude.putStrLn ("got msg: " ++ (BS.unpack msg))

    liftIO $ Prelude.putStrLn ("got msg: " ++ (BS.unpack msg))

    return ()

createAndInitActors = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable

    forkIO $ WS.runServer "127.0.0.1" 27001 $ roomApplication node

    return ()
