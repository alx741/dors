{-# LANGUAGE OverloadedStrings #-}

module Dors where

import Conduit
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever, when)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Control.Monad.Trans.State
import Data.Conduit.Process
import Data.Set                  (Set, fromList, member)
import Data.Text                 as T (Text, filter, strip, toLower, words)
import Prelude                   as P hiding (words)
import System.Posix.Signals

import Text.Mining.StopWords (StopWordsLexiconNoDiacritics,
                              readLexiconFileIgnoreDiacritics)

import Animation
import Driver
import Text.Mining.Emotion as E

tellSilence :: DorsState -> IO ()
tellSilence s@(DorsState _ mVarLastUtterance) = forever $ do
    lastUtterance <- takeMVar mVarLastUtterance
    let loop = do
            currentTime <- getCurrentTime
            print $ diffUTCTime currentTime lastUtterance
            when (diffUTCTime currentTime lastUtterance > 20) $
                print "PREGUNTA" >> tellSilence s
            loop
    loop

dors :: IO ()
dors = do
    _ <- installHandler sigINT (Catch $ robot Shutdown) Nothing

    emotionalLexicon <- liftIO $ E.loadLexiconFile "data/emotional_lexicon_es.csv"
    stopWordsLexion <- liftIO $ readLexiconFileIgnoreDiacritics "data/stopwords_es"

    (ClosedStream, speechSource, Inherited, _) <- streamingProcess $ speechCmd "data/asr_model"

    time <- getCurrentTime >>= newMVar
    let initialDorsState = DorsState Awake time
    forkIO $ tellSilence initialDorsState
    flip evalStateT initialDorsState $ runConduit
    -- flip evalStateT (DorsState Asleep 0) $ runConduit
        $  speechSource
        .| decodeUtf8C
        .| cleanUtterance
        .| handleKeywords
        .| emotionalAnalysis emotionalLexicon stopWordsLexion
        .| conveyEmotion
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
    , lastUtteranceAt :: MVar UTCTime
    } deriving (Eq)

data Wakefulness
    = Asleep
    | HalfAsleep
    | Awake
    deriving (Show, Eq)

data DorsCommand
    = WakeUp
    | Sleep
    | SayName
    | SayHello
    deriving (Show, Eq)

cleanUtterance :: ConduitT Text Text Dors ()
cleanUtterance = awaitForever $ \rawUtterance -> do
    let utterance = strip $ T.filter (/= '\n') rawUtterance
    case utterance of
        "" -> cleanUtterance
        _ -> do
            liftIO . putStrLn $ show utterance
            lift updateUtteranceTime
            yield utterance
    where
        updateUtteranceTime = do
            mVarLastUtterance <- lastUtteranceAt <$> get
            time <- liftIO getCurrentTime
            liftIO $ putMVar mVarLastUtterance time

handleKeywords :: ConduitT Text Text Dors ()
handleKeywords = awaitForever $ \utterance ->
    case findKeyword utterance >>= keywordToCommand of
        Nothing -> yield utterance
        Just cmd -> printCmd cmd >> lift (evalDorsCommand cmd)
    where
        evalDorsCommand :: DorsCommand -> Dors ()
        evalDorsCommand cmd
            | cmd == WakeUp = do
                (DorsState w c) <- get
                case w of
                    Asleep -> do
                        liftIO wakeUpPhase1
                        put $ DorsState HalfAsleep c
                    HalfAsleep -> do
                        liftIO wakeUpPhase2
                        put $ DorsState Awake c
                    Awake -> pure ()

            | cmd == Sleep    = whenAwake $ liftIO sleep
            | cmd == SayName  = whenAwake $ liftIO sayName
            | cmd == SayHello = whenAwake $ liftIO sayHello
            | otherwise = pure ()

        findKeyword :: Text -> Maybe Text
        findKeyword t =
            case P.filter (`member` keywords) $ (words . strip . toLower) t of
                [] -> Nothing
                (x:_) -> Just x

        keywordToCommand :: Text -> Maybe DorsCommand
        keywordToCommand keyword
            | keyword `elem` sleepKeywords    = Just Sleep
            | keyword `elem` wakeUpKeywords   = Just WakeUp
            | keyword `elem` sayNameKeywords  = Just SayName
            | keyword `elem` sayHelloKeywords = Just SayHello
            | otherwise = Nothing

        printCmd cmd = liftIO $ putStrLn $ "-- Handling command: " <> show cmd

        keywords :: Set Text
        keywords = fromList
            $  sleepKeywords
            <> wakeUpKeywords
            <> sayNameKeywords
            <> sayHelloKeywords

        sleepKeywords    = ["duerme", "duermete", "duermen"]
        wakeUpKeywords   = ["despierta", "despiertate", "despiertan"]
        sayNameKeywords  = ["nombre", "nombres", "llama", "llamas", "llaman"]
        sayHelloKeywords = ["hola"]


emotionalAnalysis :: E.Lexicon -> StopWordsLexiconNoDiacritics -> ConduitT Text E.Emotion Dors ()
emotionalAnalysis emotional stopwords = awaitForever $ \utterance ->
    yield $ E.utteranceEmotion emotional stopwords utterance

conveyEmotion :: ConduitT E.Emotion Void Dors ()
conveyEmotion = awaitForever $ \emotion -> lift $ whenAwake $ do
    liftIO $ putStrLn $ "-- Conveying emotion: " <> show emotion
    case emotion of
        Joy -> liftIO $ set Smiley
        Anger -> liftIO $ set Angry
        Sadness -> liftIO $ set Sad
        Fear -> liftIO $ set Sad
        _ -> liftIO $ set Neutral
    where set = setEyes
    -- where set = setEmotion

useUpEmotions :: ConduitT E.Emotion Void Dors ()
useUpEmotions = awaitForever $ \emotion ->
    liftIO $ putStrLn $ "-- Emotion: " <> show emotion

whenAwake :: Dors () -> Dors ()
whenAwake f = do
    (DorsState w _) <- get
    case w of
        Awake -> f
        _ -> pure ()
