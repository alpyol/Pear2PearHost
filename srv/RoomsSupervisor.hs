module RoomsSupervisor (supervisorProcess) where

import Control.Distributed.Process as DP

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map  as M
import qualified Data.List as L
import Data.Maybe

import ActorsMessages (FromRoomMsg(..), FromClientMsg(..), SupervisorToClientMsg(..))

data SupervisorState = SupervisorState {
    getRoomsByURL :: M.Map BS.ByteString [DP.ProcessId],
    getURLsByRoom :: M.Map DP.ProcessId  [BS.ByteString]
    } deriving (Show)

initialSupervisorState :: SupervisorState
initialSupervisorState = SupervisorState M.empty M.empty

logMessage :: BS.ByteString -> Process (Maybe SupervisorState)
logMessage msg = do
    say $ "got unhandled string: " ++ BS.unpack msg ++ "\r\n"
    return Nothing

addIfNoExists :: Eq a => a -> [a] -> [a]
addIfNoExists newEl arr = maybe (newEl:arr) (\_ -> arr) (L.find (\el -> el == newEl) arr)

putImage :: SupervisorState -> (DP.ProcessId, BS.ByteString) -> SupervisorState
putImage state (owner, url) =
    let roomsByURL  = getRoomsByURL state
        urlsByRooms = getURLsByRoom state

        processes = M.findWithDefault [] url   roomsByURL
        urls      = M.findWithDefault [] owner urlsByRooms

        newProcesses = addIfNoExists owner processes
        newUrls      = addIfNoExists url   urls
    
    in SupervisorState (M.insert url newProcesses roomsByURL) (M.insert owner newUrls urlsByRooms)

getPidForURL :: SupervisorState -> BS.ByteString -> Maybe (DP.ProcessId, SupervisorState)
getPidForURL state url =
    let roomsByURL  = getRoomsByURL state
        urlsByRooms = getURLsByRoom state

        rooms = M.findWithDefault [] url roomsByURL
        room   = listToMaybe rooms

        (first:leftRooms) = rooms
        newRoomsByURL = M.insert url (leftRooms ++ [first]) roomsByURL

    in maybe Nothing (\pid -> Just (pid, SupervisorState newRoomsByURL urlsByRooms)) room

removePids :: DP.ProcessId -> [BS.ByteString] -> M.Map BS.ByteString [DP.ProcessId] -> M.Map BS.ByteString [DP.ProcessId]
removePids _ [] state = state
removePids pid (x:xs) state =
    let pids = filter (\x -> x /= pid) $ maybe [] id (M.lookup x state)
    in removePids pid xs (if null pids then M.delete x state else M.insert x pids state)

removeRoom :: SupervisorState -> DP.ProcessId -> SupervisorState
removeRoom state roomPid =
    let urlsToRemovePids = maybe [] id (M.lookup roomPid (getURLsByRoom state))
        pidsByURL        = removePids roomPid urlsToRemovePids (getRoomsByURL state)

    in SupervisorState pidsByURL (M.delete roomPid (getURLsByRoom state))

processAddImage :: SupervisorState -> FromRoomMsg -> Process (Maybe SupervisorState)
processAddImage state (URLAddedMsg owner url) = do
    let newState = putImage state (owner, url)
    say $ "+ newState: " ++ show newState
    return $ Just newState
processAddImage state (RoomClosedMsg closedRoom) = do
    let newState = removeRoom state closedRoom
    say $ "- newState: " ++ show newState
    return $ Just newState

-- data FromClientMsg = RequestOffer DP.ProcessId BS.ByteString
processClientMsgs :: SupervisorState -> FromClientMsg -> Process (Maybe SupervisorState)
processClientMsgs state (RequestOffer client url) = do
    -- getPidForURL :: SupervisorState -> BS.ByteString -> Maybe (DP.ProcessId, SupervisorState)
    case getPidForURL state url of
        (Just (room, newState)) -> do
            -- TODO send offer request to the room
            say $ "TODO process"
            return $ Just newState
        Nothing -> do
            send client NoImageError
            return Nothing

supervisorProcess' :: SupervisorState -> Process ()
supervisorProcess' state = do
    newState <- receiveWait [match (processAddImage state), match (processClientMsgs state), match logMessage]
    supervisorProcess' $ maybe state id newState

supervisorProcess :: Process ()
supervisorProcess = supervisorProcess' initialSupervisorState 
