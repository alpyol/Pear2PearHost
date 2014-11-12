{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}

module ActorsMessages (
    FromRoomMsg(..),
    SocketMsg(..),
    FromClientMsg(..),
    SupervisorToClientMsg(..)
    ) where

import Control.Distributed.Process as DP

import Data.Binary
import Data.Typeable
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.DeriveTH

data FromRoomMsg = URLAddedMsg DP.ProcessId BS.ByteString | RoomClosedMsg DP.ProcessId
    deriving (Show, Typeable {-!, Binary !-})
$( derive makeBinary ''FromRoomMsg )

data FromClientMsg = RequestOffer DP.ProcessId BS.ByteString
    deriving (Show, Typeable {-!, Binary !-})
$( derive makeBinary ''FromClientMsg )

data SupervisorToClientMsg = NoImageError
    deriving (Show, Typeable {-!, Binary !-})
$( derive makeBinary ''SupervisorToClientMsg )

data SocketMsg = SocketMsg BS.ByteString | CloseMsg deriving (Show, Typeable {-!, Binary !-})
$( derive makeBinary ''SocketMsg )
