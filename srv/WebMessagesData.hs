{-# LANGUAGE OverloadedStrings #-}

module WebMessagesData (
    ClientOffer(..),
    ClientCandidate(..),
    ClientError(..),
    toJSON) where

import Data.Aeson.Types
import Data.Text

data ClientOffer = ClientOffer Text

instance ToJSON ClientOffer where
    toJSON (ClientOffer offer) = object ["msgType" .= ("Offer" :: Text), "offer" .= offer]

data ClientCandidate = ClientCandidate Text

instance ToJSON ClientCandidate where
    toJSON (ClientCandidate candidate) = object ["msgType" .= ("Candidate" :: Text), "candidate" .= candidate]

data ClientError = ClientError Text

instance ToJSON ClientError where
    toJSON (ClientError message) = object ["msgType" .= ("Error" :: Text), "candidate" .= message]
