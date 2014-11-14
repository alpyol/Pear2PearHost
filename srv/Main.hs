{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.IORef (IORef, newIORef, atomicWriteIORef, readIORef)
import Yesod
import Data.Text hiding (count)

import Actors

data HelloWorld = HelloWorld {
    pearOfferRef :: IORef Text
}

mkYesod "HelloWorld" [parseRoutes|
/pearOffer PearOfferR GET POST -- (4)
|]

instance Yesod HelloWorld

putPearOffer :: IORef Text -> Text -> IO ()
putPearOffer = atomicWriteIORef

getPearOffer :: IORef Text -> IO Text
getPearOffer = readIORef

postPearOfferR :: Handler Value
postPearOfferR = do
    yesod        <- getYesod
    pearOfferVal <- lookupPostParam "pear_offer"
    status <- liftIO $ case pearOfferVal of
            Just val -> do
                putPearOffer (pearOfferRef yesod) val
                return $ pack "ok"
            Nothing  -> return $ pack "fails"
    returnJson $ toJSON status

getPearOfferR :: Handler Value
getPearOfferR = do 
    yesod  <- getYesod
    result <- liftIO $ getPearOffer $ pearOfferRef yesod
    returnJson $ toJSON result

main :: IO ()
main = do
    pearOffer <- newIORef ""

    createAndInitActors

    warp 3002 HelloWorld { pearOfferRef = pearOffer }
