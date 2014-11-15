{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}

module ActorsMessages (
    FromRoomMsg(..),
    ClientToRoomMsg(..),
    ClientToSupervisorMsg(..),
    SupervisorToClientMsg(..),
    RoomToClientMsg(..),
    ImgSrvToClientMsg(..),
    ClientToImgSrvMsg(..)
    ) where

import Control.Distributed.Process as DP

import Data.Binary
import Data.Typeable
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.DeriveTH

data FromRoomMsg = URLAddedMsg DP.ProcessId BS.ByteString | RoomClosedMsg DP.ProcessId
    deriving (Show, Typeable {-!, Binary !-})
$( derive makeBinary ''FromRoomMsg )

data ClientToRoomMsg = RequestOffer DP.ProcessId BS.ByteString
    deriving (Show, Typeable {-!, Binary !-})
$( derive makeBinary ''ClientToRoomMsg )

data ClientToImgSrvMsg = SendAnswer BS.ByteString | SendRemoteIceCandidate BS.ByteString
    deriving (Show, Typeable {-!, Binary !-})
$( derive makeBinary ''ClientToImgSrvMsg )

data ClientToSupervisorMsg = GetRoom DP.ProcessId BS.ByteString
    deriving (Show, Typeable {-!, Binary !-})
$( derive makeBinary ''ClientToSupervisorMsg )

data SupervisorToClientMsg = NoImageError | URLRoom DP.ProcessId
    deriving (Show, Typeable {-!, Binary !-})
$( derive makeBinary ''SupervisorToClientMsg )

data RoomToClientMsg = NoImageOnWebError
    deriving (Show, Typeable {-!, Binary !-})
$( derive makeBinary ''RoomToClientMsg )

data ImgSrvToClientMsg = Offer DP.ProcessId BS.ByteString | Candidate DP.ProcessId BS.ByteString
    deriving (Show, Typeable {-!, Binary !-})
$( derive makeBinary ''ImgSrvToClientMsg )
