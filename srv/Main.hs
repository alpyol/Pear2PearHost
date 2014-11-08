{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Data.IORef (IORef, newIORef, atomicWriteIORef, readIORef)             
import Yesod
import Data.Text hiding (count)

data HelloWorld = HelloWorld {
    pearOfferRef :: IORef (Text) -- (1)
}

mkYesod "HelloWorld" [parseRoutes|
/pearOffer PearOfferR GET POST -- (4)
|]

instance Yesod HelloWorld

putPearOffer :: IORef Text -> Text -> IO ()
putPearOffer pearOfferRef value = atomicWriteIORef pearOfferRef value

getPearOffer :: IORef Text -> IO Text
getPearOffer = readIORef

postPearOfferR :: Handler Value
postPearOfferR = do
    yesod        <- getYesod
    pearOfferVal <- lookupPostParam "pear_offer"
    status <- liftIO $ do
        case pearOfferVal of
            Just val -> do
                putPearOffer (pearOfferRef yesod) val
                return $ pack "ok"
            Nothing  -> 
                return $ pack "fails"
    returnJson $ toJSON status

getPearOfferR :: Handler Value
getPearOfferR = do 
    yesod  <- getYesod
    result <- liftIO $ getPearOffer $ pearOfferRef yesod
    --liftIO $ putStrLn $ "Sending Response " ++ show count -- (8)
    returnJson $ toJSON result

main :: IO ()
main = do
    pearOffer <- newIORef ""
    warp 3002 $ HelloWorld { pearOfferRef = pearOffer } -- (3)