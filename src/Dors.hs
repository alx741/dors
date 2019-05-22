{-# LANGUAGE OverloadedStrings #-}

module Dors where

import           Conduit
import Data.Text
import           SttClient
import qualified Text.Mining.Emotion as E

sttConfig :: ClientConfig
sttConfig = ClientConfig "es-ES_BroadbandModel" 0.15

buildUtterance :: ConduitT Text Text IO ()
buildUtterance = buildUp ""
    where
        buildUp utterance = do
            mVal <- await
            case mVal of
                Nothing -> do
                    liftIO $ print "Nothing"
                    pure ()
                Just val
                    | val == "" -> yield utterance >> buildUtterance
                    | otherwise -> buildUp $ utterance <> val

