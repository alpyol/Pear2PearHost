module Actors (createAndInitActors) where

import Control.Distributed.Process.Node
import Control.Concurrent

import Network.Transport.TCP

import RoomsSupervisor   (supervisorProcess)
import RoomActors        (forkRoomServer)
import ParticipantActors (forkParticipantServer)
import ImageServerActor  (forkImageServer)

createAndInitActors = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable

    supervisor <- forkProcess node supervisorProcess

    forkRoomServer        node supervisor
    forkParticipantServer node supervisor
    forkImageServer       node

    return ()
