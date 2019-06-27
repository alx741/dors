{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Dors where

import Conduit
import Control.Concurrent         (forkIO, killThread, newEmptyMVar, putMVar)
import Control.Concurrent.MVar    (takeMVar)
import Control.Monad              (forever)
import Control.Monad.Trans.State
import Data.ByteString            (ByteString, hGetLine)
import Data.Conduit.Process
import Data.Set                   (Set, fromList, member)
import Data.Text                  as T (Text, filter, strip, toLower, words)
import Data.Time.Clock            (getCurrentTime)
import Database.PostgreSQL.Simple
import Prelude                    as P hiding (words)
import System.IO                  (BufferMode (..), hSetBuffering)
import System.Posix.Signals
import System.Random              (randomRIO)
import System.Timeout             (timeout)

import Text.Mining.StopWords (StopWordsLexiconNoDiacritics,
                              readLexiconFileIgnoreDiacritics)

import Animation
import Driver
import Inquire
import Text.Mining.Emotion as E

import Api


dors :: [String] -> IO ()
dors args = do

    initialState  <-
            case args of
                ("-a":_) -> goNeutral >> pure (DorsState Awake)
                _        -> pure (DorsState Asleep)

    installHandler sigINT (Catch $ robot Shutdown) Nothing

    emotionalLexicon <- liftIO $ E.loadLexiconFile "data/emotional_lexicon_es.csv"
    stopWordsLexion <- liftIO $ readLexiconFileIgnoreDiacritics "data/stopwords_es"
    questions <- liftIO $ loadQuestions "data/questions"

    forkIO runAPI

    putStrLn $ "*-- Initial State: " <> show initialState

    flip evalStateT initialState $ runConduit
        $  sourceUtterance 10 questions
        .| decodeUtf8C
        .| cleanUtterance
        -- .| handleKeywords
        .| emotionalAnalysis emotionalLexicon stopWordsLexion
        -- .| conveyEmotion
        -- .| storeEmotion
        .| useUpEmotions

type Dors = StateT DorsState IO

data DorsState = DorsState
    { wakefulness :: Wakefulness
    } deriving (Eq, Show)

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

sourceUtterance :: Int -> [Text] -> ConduitT () ByteString Dors ()
sourceUtterance s questions = do
    (_, Just outp, _, _phandle) <- liftIO $ createProcess $ (speechCmd "data/asr_model")  { std_out = CreatePipe, std_in = CreatePipe }
    liftIO $ hSetBuffering outp LineBuffering
    mvar <- liftIO $ newEmptyMVar
    tid <- liftIO $ forkIO $ forever $ hGetLine outp >>= putMVar mvar

    let loop qs = do
            result <- liftIO $ timeout (seconds s) (takeMVar mvar)
            case result of
                Nothing -> do
                    qs' <- lift $ askQuestion qs
                    loop qs'
                Just x -> yield x >> loop qs

    loop questions
    liftIO $ killThread tid
    where
        askQuestion :: [Text] -> Dors [Text]
        askQuestion questions = do
            (DorsState w) <- get
            case w of
                Awake -> do
                    liftIO $ putStrLn $
                        "-- Asking a question. Questions left: "
                        <> show (length questions - 1)
                    liftIO $ askRandomQuestion questions
                _ -> pure questions

        seconds s = s * 1000000

        speechCmd modelDir = shell
            $  "pocketsphinx_continuous"
            <> " -hmm " <> modelDir
            <> " -lm " <> modelDir <> "/es-20k.lm"
            <> " -dict " <> modelDir <> "/es.dict"
            <> " -inmic " <> "yes"
            <> " 2> stt.log"

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
    liftIO $ execute dbConn "insert into emotions (emotion, at) values (?, ?)" (show emotion, time)
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
