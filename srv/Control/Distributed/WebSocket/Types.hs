{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Control.Distributed.WebSocket.Types where

import Control.Distributed.Process as DP

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Word
import Data.Binary
import Data.Typeable
import Data.DeriveTH

data Send = SendTextData BS.ByteString
    deriving (Show, Typeable {-!, Binary !-})
$( derive makeBinary ''Send )

data Receive = Opened DP.ProcessId | Text BS.ByteString | Binary BS.ByteString | Close Word16 BS.ByteString
    deriving (Show, Typeable {-!, Binary !-})
$( derive makeBinary ''Receive )

-- data FromRoomMsg = URLAddedMsg DP.ProcessId BS.ByteString | RoomClosedMsg DP.ProcessId
--     deriving (Show, Typeable {-!, Binary !-})
-- $( derive makeBinary ''FromRoomMsg )
