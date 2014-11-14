{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Control.Distributed.WebSocket.Types where

import Control.Distributed.Process as DP

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Word
import Data.Binary
import Data.Typeable
import Data.DeriveTH

data Send = SendTextData BS.ByteString | Close BS.ByteString
    deriving (Show, Typeable {-!, Binary !-})
$( derive makeBinary ''Send )

data Receive = Text BS.ByteString | Binary BS.ByteString | Closed Word16 BS.ByteString
    deriving (Show, Typeable {-!, Binary !-})
$( derive makeBinary ''Receive )
