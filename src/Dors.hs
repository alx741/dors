{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Dors where

import Conduit
import Control.Concurrent         (forkIO)
import Control.Monad.Trans.State
import Data.Conduit.Process
import Data.Set                   (Set, fromList, member)
import Data.Text                  as T (Text, filter, strip, toLower, words)
import Data.Time.Clock            (getCurrentTime)
import Database.PostgreSQL.Simple
import Prelude                    as P hiding (words)
import System.Posix.Signals
import System.Random              (randomRIO)

import Text.Mining.StopWords (StopWordsLexiconNoDiacritics,
                              readLexiconFileIgnoreDiacritics)

import Animation
import Driver
import Text.Mining.Emotion as E

import Api

dors :: IO ()
dors = do
    _ <- installHandler sigINT (Catch $ robot Shutdown) Nothing

    emotionalLexicon <- liftIO $ E.loadLexiconFile "data/emotional_lexicon_es.csv"
    stopWordsLexion <- liftIO $ readLexiconFileIgnoreDiacritics "data/stopwords_es"

    (ClosedStream, speechSource, Inherited, _) <- streamingProcess $ speechCmd "data/asr_model"

    _ <- forkIO runAPI

    -- flip evalStateT (DorsState Awake) $ runConduit
    flip evalStateT (DorsState Asleep) $ runConduit
        $  speechSource
        .| decodeUtf8C
        .| cleanUtterance
        .| handleKeywords
        .| emotionalAnalysis emotionalLexicon stopWordsLexion
        .| conveyEmotion
        .| storeEmotion
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

            | cmd == Sleep    = whenAwake $ liftIO sleep
            | cmd == SayName  = whenAwake $ liftIO sayName
            | cmd == SayHello = whenAwake $ liftIO sayHello
            | otherwise = pure ()

        findKeyword :: Text -> Maybe Text
        findKeyword t =
            case P.filter (`member` keywords) $ (words . strip . toLower) t of
                []    -> Nothing
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
        wakeUpKeywords   = ["despierta", "despierto", "despiertate", "despiertan"]
        sayNameKeywords  = ["nombre", "nombres", "llama", "llamas", "llaman"]
        sayHelloKeywords = ["hola"]


emotionalAnalysis :: E.Lexicon -> StopWordsLexiconNoDiacritics -> ConduitT Text E.Emotion Dors ()
emotionalAnalysis emotional stopwords = awaitForever $ \utterance ->
    yield $ E.utteranceEmotion emotional stopwords utterance

conveyEmotion :: ConduitT E.Emotion E.Emotion Dors ()
conveyEmotion = awaitForever $ \emotion -> convey emotion >> yield emotion
    where
        convey emotion = lift $ whenAwake $ liftIO $ do
            putStrLn $ "-- Conveying emotion: " <> show emotion
            rndPickPhyEmotion emotion >>= setEmotion

        rndPickPhyEmotion :: E.Emotion -> IO PhyEmotion
        rndPickPhyEmotion e =
            let emotions = physicalEmotions e
            in (emotions !!) <$> randomRIO (0, length emotions - 1)

storeEmotion :: ConduitT E.Emotion Void Dors ()
storeEmotion = awaitForever $ \emotion -> do
    dbConn <- liftIO $ connectPostgreSQL "host=localhost port=5432 dbname=dors user=alx password=verde"
    time <- liftIO getCurrentTime
    _ <- liftIO $ execute dbConn "insert into emotions (emotion, at) values (?, ?)" (show emotion, time)
    pure ()

useUpEmotions :: ConduitT E.Emotion Void Dors ()
useUpEmotions = awaitForever $ \emotion ->
    liftIO $ putStrLn $ "-- Emotion: " <> show emotion

whenAwake :: Dors () -> Dors ()
whenAwake f = do
    (DorsState w) <- get
    case w of
        Awake -> f
        _     -> pure ()


physicalEmotions :: E.Emotion -> [PhyEmotion]
physicalEmotions Anticipation = [Surprised, Suspicious]
physicalEmotions Joy          = [Happy, Smiley, Love]
physicalEmotions Trust        = [Happy, Neutral]
physicalEmotions Fear         = [Sad, Confused, Surprised]
physicalEmotions Sadness      = [Sad]
physicalEmotions Disgust      = [Angry, Surprised, Confused]
physicalEmotions Anger        = [Angry]
physicalEmotions Surprise     = [Surprised]
physicalEmotions _            = [Neutral]
