{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Actors (createAndInitActors) where

import Control.Distributed.Process as DP
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable
import Network.Transport.TCP

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent

import RoomActors

createAndInitActors = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable

    forkIO $ runRoomServer node

    return ()
