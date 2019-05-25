{-# LANGUAGE OverloadedStrings #-}

module Dors where

import Conduit
import Control.Monad.Trans.State
import Data.Conduit.Process
import Data.Set                  (Set, fromList, member)
import Data.Text                 as T (Text, filter, strip, toLower, words)
import Prelude                   as P hiding (words)

import Text.Mining.StopWords (StopWordsLexiconNoDiacritics,
                              readLexiconFileIgnoreDiacritics)

import Animation
import Driver
import Text.Mining.Emotion as E

dors :: IO ()
dors = do
    emotionalLexicon <- liftIO $ E.loadLexiconFile "data/emotional_lexicon_es.csv"
    stopWordsLexion <- liftIO $ readLexiconFileIgnoreDiacritics "data/stopwords_es"
    (ClosedStream, speechSource, Inherited, _) <- streamingProcess $ speechCmd "data/asr_model"
    flip evalStateT (DorsState Asleep) $ runConduit
        $  speechSource
        .| decodeUtf8C
        .| cleanUtterance
        .| handleKeywords
        .| emotionalAnalysis emotionalLexicon stopWordsLexion
        .| conveyEmotion
        -- .| useUpEmotions
    where
        speechCmd modelDir = shell
            $  "pocketsphinx_continuous"
            <> " -hmm " <> modelDir
            <> " -lm " <> modelDir <> "/es-20k.lm"
            <> " -dict " <> modelDir <> "/es.dict"
            <> " -inmic " <> "yes"
            <> " 2> stt.log"

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

cleanUtterance :: ConduitT Text Text Dors ()
cleanUtterance = awaitForever $ \rawUtterance -> do
    let utterance = strip $ T.filter (/= '\n') rawUtterance
    case utterance of
        "" -> cleanUtterance
        _ -> do
            liftIO . putStrLn $ show utterance
            yield utterance


handleKeywords :: ConduitT Text Text Dors ()
handleKeywords = awaitForever $ \utterance ->
    case findKeyword utterance >>= keywordToCommand of
        Nothing  -> yield utterance
        Just cmd -> printCmd cmd >> lift (evalDorsCommand cmd)
    where
        evalDorsCommand :: DorsCommand -> Dors ()
        evalDorsCommand cmd
            | cmd == WakeUp = do
                (DorsState w) <- get
                case w of
                    Asleep -> do
                        liftIO wakeUpPhase1
                        put $ DorsState HalfAsleep
                    HalfAsleep -> do
                        liftIO wakeUpPhase2
                        put $ DorsState Awake
                    Awake -> pure ()

            | cmd == Sleep = whenAwake $ liftIO sleep

            | cmd == SayName = whenAwake $ liftIO sayName

            | otherwise = pure ()

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

        printCmd cmd = liftIO $ putStrLn $ "-- Handling command: " <> show cmd

        keywords :: Set Text
        keywords = fromList
            $  sleepKeywords
            <> wakeUpKeywords
            <> sayNameKeywords

        sleepKeywords   = ["duerme", "duermete", "duermen"]
        wakeUpKeywords  = ["despierta", "despiertate", "despiertan"]
        sayNameKeywords = ["nombre", "nombres", "llama", "llamas", "llaman"]



emotionalAnalysis :: E.Lexicon -> StopWordsLexiconNoDiacritics -> ConduitT Text E.Emotion Dors ()
emotionalAnalysis emotional stopwords = awaitForever $ \utterance ->
    yield $ E.utteranceEmotion emotional stopwords utterance

conveyEmotion :: ConduitT E.Emotion Void Dors ()
conveyEmotion = awaitForever $ \emotion -> lift $ whenAwake $ do
    liftIO $ putStrLn $ "-- Conveying emotion: " <> show emotion
    case emotion of
        Joy     -> liftIO $ setEmotion Smiley
        Anger   -> liftIO $ setEmotion Angry
        Sadness -> liftIO $ setEmotion Sad
        Fear    -> liftIO $ setEmotion Sad
        _       -> liftIO $ setEmotion Neutral

useUpEmotions :: ConduitT E.Emotion Void Dors ()
useUpEmotions = awaitForever $ \emotion ->
    liftIO $ putStrLn $ "-- Emotion: " <> show emotion


whenAwake :: Dors () -> Dors ()
whenAwake f = do
    (DorsState w) <- get
    case w of
        Awake -> f
        _     -> pure ()
