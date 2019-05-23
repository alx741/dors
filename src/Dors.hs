{-# LANGUAGE OverloadedStrings #-}

module Dors where

import Conduit
import Control.Monad             (forever)
import Control.Monad.Trans.State
import Data.Set                  as S
import Data.Text                 (Text, strip, toLower, words)
import Prelude                   as P hiding (words)

import SttClient
import Text.Mining.StopWords (readLexiconFileIgnoreDiacritics)

import Driver
import Text.Mining.Emotion as E

runDors :: FilePath -> IO ()
runDors accessTokenFile = do
    token <- P.filter (/= '\n') <$> readFile accessTokenFile
    runWithSpeech sttConfig token dors
    where
        sttConfig :: ClientConfig
        sttConfig = ClientConfig "es-ES_BroadbandModel" 0.15

dors :: Connection -> IO ()
dors conn =
    flip evalStateT (DorsState Asleep) $ runConduit $ speechSource conn
        .| buildUtterance
        .| handleKeywords
        .| emotionalAnalysis "data/emotional_lexicon_es.csv" "data/stopwords_es"
        -- .| conveyEmotion
        .| useUpEmotions


type Dors = StateT DorsState IO

data DorsState = DorsState
    { wakefulness :: Wakefulness
    } deriving (Show, Eq)

data Wakefulness
    = Asleep
    | HalfAsleep
    | Awake
    deriving (Show, Eq)

data DorsCommand
    = WakeUp
    | Sleep
    | SayName
    deriving (Show, Eq)

speechSource :: Connection -> ConduitT () Text Dors ()
speechSource conn = forever $ yieldM $ liftIO $ receiveTranscripts conn

buildUtterance :: ConduitT Text Text Dors ()
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


handleKeywords :: ConduitT Text Text Dors ()
handleKeywords = awaitForever $ \utterance ->
    case findKeyword utterance >>= keywordToCommand of
        Nothing  -> yield utterance
        Just cmd -> liftIO $ evalDorsCommand cmd
    where
        findKeyword :: Text -> Maybe Text
        findKeyword t =
            case P.filter (`member` keywords) $ (words . strip . toLower) t of
                []    -> Nothing
                (x:_) -> Just x

        keywordToCommand :: Text -> Maybe DorsCommand
        keywordToCommand keyword
            | keyword `elem` sleepKeywords   = Just Sleep
            | keyword `elem` wakeUpKeywords  = Just WakeUp
            | keyword `elem` sayNameKeywords = Just SayName
            | otherwise = Nothing

        evalDorsCommand :: DorsCommand -> IO ()
        evalDorsCommand cmd = print $ "-- CMD: " <> show cmd

        keywords :: Set Text
        keywords = S.fromList
            $  sleepKeywords
            <> wakeUpKeywords
            <> sayNameKeywords

        sleepKeywords = ["duerme", "duermete"]
        wakeUpKeywords = ["despierta", "despiertate"]
        sayNameKeywords = ["nombre", "nombres", "llama", "llamas"]



emotionalAnalysis :: FilePath -> FilePath -> ConduitT Text E.Emotion Dors ()
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

conveyEmotion :: ConduitT E.Emotion Void Dors ()
conveyEmotion =
    awaitForever $ \emotion -> do
        liftIO $ print emotion
        case emotion of
            Joy     -> liftIO $ setEmotion Smiley
            Anger   -> liftIO $ setEmotion Angry
            Sadness -> liftIO $ setEmotion Sad
            Fear    -> liftIO $ setEmotion Sad
            _       -> liftIO $ setEmotion Neutral

useUpEmotions :: ConduitT E.Emotion Void Dors ()
useUpEmotions = do
    mval <- await
    liftIO $ print mval
    useUpEmotions
