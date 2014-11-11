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

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M

import RoomActors
import ActorsMessages (URLAddedMessage)

data SupervisorState = SupervisorState { getSupervisorState :: M.Map BS.ByteString [DP.ProcessId] }

initialSupervisorState :: SupervisorState
initialSupervisorState = SupervisorState M.empty

logMessage :: BS.ByteString -> Process (Maybe SupervisorState)
logMessage msg = do
    say $ "got unhandled string: " ++ BS.unpack msg ++ "\r\n"
    return Nothing

processAddImageCmd :: URLAddedMessage -> SupervisorState -> Process (Maybe SupervisorState)
processAddImageCmd newImage state = undefined

createAndInitActors = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable

    forkIO $ runRoomServer node

    return ()
