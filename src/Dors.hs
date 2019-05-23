{-# LANGUAGE OverloadedStrings #-}

module Dors where

import Conduit
import Control.Monad             (forever)
import Control.Monad.Trans.State
import Data.Text

import SttClient
import Text.Mining.StopWords (readLexiconFileIgnoreDiacritics)

import Driver
import Text.Mining.Emotion as E

runDors :: FilePath -> IO ()
runDors accessTokenFile = do
    token <- Prelude.filter (/= '\n') <$> readFile accessTokenFile
    runWithSpeech sttConfig token dors
    where
        sttConfig :: ClientConfig
        sttConfig = ClientConfig "es-ES_BroadbandModel" 0.15

dors :: Connection -> IO ()
dors conn =
    runConduit $ speechSource conn
        .| buildUtterance
        .| emotionalAnalysis "data/emotional_lexicon_es.csv" "data/stopwords_es"
        -- .| conveyEmotion
        .| useUpEmotions


speechSource :: Connection -> ConduitT () Text IO ()
speechSource conn = forever $ yieldM $ receiveTranscripts conn

data DorsState = DorsState
    { wakefulness :: Wakefulness
    } deriving (Show, Eq)

data Wakefulness
    = Asleep
    | HalfAsleep
    | Awake
    deriving (Show, Eq)

type Dors = StateT DorsState IO ()

-- ConduitT () Void Dors

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
conveyEmotion =
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
