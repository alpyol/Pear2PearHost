module Actors (createAndInitActors) where

import Control.Distributed.Process.Node
import Control.Concurrent

import Network.Transport.TCP

import RoomActors (runRoomServer)
import ParticipantActors (runParticipantServer)
import RoomsSupervisor (supervisorProcess)

createAndInitActors = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable

    supervisorProcessID <- forkProcess node supervisorProcess

    forkIO $ runRoomServer node supervisorProcessID
    forkIO $ runParticipantServer node supervisorProcessID

    return ()
