{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ActorsCmn (jsonObjectWithType, withCpid, pid2Str, str2Pid) where

import Control.Distributed.Process as DP

import qualified Data.Binary as BN
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Aeson as AES ((.:), decode)
import Data.Aeson.Types

jsonObjectWithType :: BS.ByteString -> Either String (String, Object)
jsonObjectWithType jsonStr = 
    case AES.decode jsonStr :: Maybe Value of
        (Just (Object json)) -> do
            let cmdType :: Maybe String = parseMaybe (.: "msgType") json
            case cmdType of
                (Just someCmd) -> Right (someCmd, json)
                Nothing -> Left $ "no command in json: " ++ show json
        (Just jsonVal) -> Left $ "got unsupported json object: " ++ show jsonVal
        Nothing -> Left $ "can not parse json object: " ++ show (BS.unpack jsonStr)

withCpid :: Object -> a -> (DP.ProcessId -> Process a) -> Process a
withCpid json defValue handler' = do
    -- {"msgType":"...","cpid":"pid://127.0.0.1:10501:0:17"}
    let clientStr :: Maybe String = (parseMaybe (.: "cpid") json)
    case clientStr of
        (Just clientStr) ->
            case str2Pid clientStr of
                (Right client) -> handler' client
                (Left err) -> do
                    return defValue
        Nothing -> do
            return defValue
pid2Str :: DP.ProcessId -> String
pid2Str = BS.unpack . B64.encode . BN.encode

str2Pid :: String -> Either String DP.ProcessId
str2Pid str = do
    case B64.decode $ BS.pack str of
        (Right decoded) -> case (BN.decodeOrFail decoded) of
            (Right (_, _, res  )) -> Right res
            (Left  (_, _, descr)) -> Left $ "can not parse pid: " ++ (BS.unpack decoded) ++ " error: " ++ descr
        (Left  err) -> Left $ "can not parse base64: " ++ err
