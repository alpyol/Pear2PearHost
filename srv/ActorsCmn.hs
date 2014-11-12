{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ActorsCmn (jsonObjectWithType) where

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
