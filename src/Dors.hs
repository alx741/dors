{-# LANGUAGE OverloadedStrings #-}

module Dors where

import Conduit
import Data.Text

import Text.Mining.StopWords (readLexiconFileIgnoreDiacritics)

import           SttClient
import qualified Text.Mining.Emotion as E

runDors :: FilePath -> IO ()
runDors accessTokenFile = do
    token <- Prelude.filter (/= '\n') <$> readFile accessTokenFile
    runPipelineWithSpeech sttConfig token pipeline

sttConfig :: ClientConfig
sttConfig = ClientConfig "es-ES_BroadbandModel" 0.15

buildUtterance :: ConduitT Text Text IO ()
buildUtterance = buildUp ""
    where
        buildUp utterance = do
            mVal <- await
            case mVal of
                Nothing -> pure ()
                Just ""
                    | utterance == "" -> buildUtterance
                    | otherwise -> yield utterance >> buildUtterance
                Just val -> buildUp $ utterance <> val


emotionalAnalysis :: FilePath -> FilePath -> ConduitT Text E.Emotion IO ()
emotionalAnalysis emotional stopwords= do
    emotionalLexicon <- liftIO $ E.loadLexiconFile emotional
    stopWordsLexion <- liftIO $ readLexiconFileIgnoreDiacritics stopwords
    let emotion = E.utteranceEmotion emotionalLexicon stopWordsLexion
    mUtterance <- await
    case mUtterance of
        Nothing -> do
            liftIO $ print "Nothing"
            pure ()
        Just utterance -> do
            liftIO $ print utterance
            yield $ emotion utterance
            emotionalAnalysis emotional stopwords

useUpEmotions :: ConduitT E.Emotion Void IO ()
useUpEmotions = do
    mval <- await
    liftIO $ print mval
    useUpEmotions

pipeline :: Pipeline
pipeline = buildUtterance .| emotionalAnalysis "data/emotional_lexicon_es.csv" "data/stopwords_es" .| useUpEmotions
