{-# LANGUAGE OverloadedStrings #-}

module Dors where

import Conduit
import Data.Text

import Text.Mining.StopWords (readLexiconFileIgnoreDiacritics)

import           SttClient
import Driver
import Text.Mining.Emotion as E

runDors :: FilePath -> IO ()
runDors accessTokenFile = do
    token <- Prelude.filter (/= '\n') <$> readFile accessTokenFile
    runPipelineWithSpeech sttConfig token pipeline

pipeline :: Pipeline
pipeline = buildUtterance .| emotionalAnalysis "data/emotional_lexicon_es.csv" "data/stopwords_es" .| conveyEmotion

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


-- handleKeywords :: ConduitT Text Text IO ()
-- handleKeywords = do
--     mUtterance <- await
--     case mUtterance of
--         Nothing -> pure ()
--         Just utterance
--             | utterance == "" -> buildUtterance
--             | otherwise -> yield utterance >> buildUtterance


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

conveyEmotion :: ConduitT E.Emotion Void IO ()
conveyEmotion = do
    awaitForever $ \emotion -> do
        liftIO $ print emotion
        case emotion of
            Joy     -> liftIO $ setEmotion Smiley
            Anger   -> liftIO $ setEmotion Angry
            Sadness -> liftIO $ setEmotion Sad
            Fear    -> liftIO $ setEmotion Sad
            _       -> liftIO $ setEmotion Neutral

useUpEmotions :: ConduitT E.Emotion Void IO ()
useUpEmotions = do
    mval <- await
    liftIO $ print mval
    useUpEmotions
