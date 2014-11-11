module RoomsSupervisor (supervisorProcess) where

import Control.Distributed.Process as DP

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map  as M
import qualified Data.List as L

import ActorsMessages (FromRoomMsg(..))

data SupervisorState = SupervisorState {
    getProcessesByURL :: M.Map BS.ByteString [DP.ProcessId],
    getURLsByProcess  :: M.Map DP.ProcessId  [BS.ByteString] -- Ordered
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
        let processesByURL = getProcessesByURL state
            urlsByProcess  = getURLsByProcess  state

            processes = M.findWithDefault [] url   processesByURL
            urls      = M.findWithDefault [] owner urlsByProcess

            newProcesses = addIfNoExists owner processes
            newUrls      = addIfNoExists url   urls
        
        in SupervisorState (M.insert url newProcesses processesByURL) (M.insert owner newUrls urlsByProcess)

removePids :: DP.ProcessId -> [BS.ByteString] -> M.Map BS.ByteString [DP.ProcessId] -> M.Map BS.ByteString [DP.ProcessId]
removePids _ [] state = state
removePids pid (x:xs) state =
    let pids = filter (\x -> x /= pid) $ maybe [] id (M.lookup x state)
    in removePids pid xs (if null pids then M.delete x state else M.insert x pids state)

removeRoom :: SupervisorState -> DP.ProcessId -> SupervisorState
removeRoom state roomPid =
    let urlsToRemovePids = maybe [] id (M.lookup roomPid (getURLsByProcess state))
        pidsByURL        = removePids roomPid urlsToRemovePids (getProcessesByURL state)

    in SupervisorState pidsByURL (M.delete roomPid (getURLsByProcess state))

processAddImage :: SupervisorState -> FromRoomMsg -> Process (Maybe SupervisorState)
processAddImage state (URLAddedMsg owner url) = do
    let newState = putImage state (owner, url)
    say $ "+ newState: " ++ show newState
    return $ Just newState
processAddImage state (RoomClosedMsg closedRoom) = do
    let newState = removeRoom state closedRoom
    say $ "- newState: " ++ show newState
    return $ Just newState

supervisorProcess' :: SupervisorState -> Process ()
supervisorProcess' state = do
    newState <- receiveWait [match (processAddImage state), match logMessage]
    supervisorProcess' $ maybe state id newState

supervisorProcess :: Process ()
supervisorProcess = supervisorProcess' initialSupervisorState 
