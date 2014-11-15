{-# LANGUAGE OverloadedStrings #-}

module WebMessagesData (
    ClientOffer(..),
    ClientCandidate(..),
    ClientError(..),
    ClientNoImageError(..),
    RequestOffer(..),
    Answer(..),
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
    toJSON (ClientError message) = object ["msgType" .= ("Error" :: Text), "msg" .= message]

data ClientNoImageError = ClientNoImageError

instance ToJSON ClientNoImageError where
    toJSON ClientNoImageError = object ["msgType" .= ("NoRequestedURL" :: Text)]

data RequestOffer = RequestOffer Text Text

instance ToJSON RequestOffer where
    toJSON (RequestOffer url cpid) = object ["msgType" .= ("RequestOffer" :: Text), "url" .= url, "cpid" .= cpid]

data Answer = Answer Text

instance ToJSON Answer where
    toJSON (Answer answer) = object ["msgType" .= ("Answer" :: Text), "answer" .= answer]
