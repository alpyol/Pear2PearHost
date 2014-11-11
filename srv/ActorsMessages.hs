{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}

module ActorsMessages (URLAddedMessage(..)) where

import Control.Distributed.Process as DP

import Data.Binary
import Data.Typeable
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.DeriveTH

data URLAddedMessage = URLAddedMessage DP.ProcessId BS.ByteString deriving (Show, Typeable {-!, Binary !-})
$( derive makeBinary ''URLAddedMessage )
