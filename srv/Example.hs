{-# LANGUAGE OverloadedStrings #-}

import Data.Functor
import Data.Text (Text)
import Control.Monad.IO.Class
import Control.Exception
import Control.Concurrent
import Network.WebSockets

main :: IO ()
main = runServer "127.0.0.1" 8000 client

client :: Request -> WebSockets Hybi10 ()
client req = do
  acceptRequest req
  readerLoop

readerLoop :: WebSockets Hybi10 ()
readerLoop = loop
  where
  loop = do
    msg <- catchWsError (Just <$> receive) $
            \e -> liftIO (print e) >> return Nothing
    liftIO $ print msg
    case msg of
      Nothing -> return ()  -- connection closed
      Just _ -> do
        sendTextData ("OK" :: Text)
        loop