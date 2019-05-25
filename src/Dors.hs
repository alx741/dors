{-# LANGUAGE OverloadedStrings #-}

module Dors where

import Conduit
import Control.Monad.Trans.State
import Data.Conduit.Process
import Data.Set                  (Set, fromList, member)
import Data.Text                 as T (Text, filter, strip, toLower, words)
import Prelude                   as P hiding (words)

import Text.Mining.StopWords (readLexiconFileIgnoreDiacritics)

import Animation
import Driver
import Text.Mining.Emotion as E

dors :: IO ()
dors = do
    (ClosedStream, speechSource, Inherited, cph) <- streamingProcess (shell "pocketsphinx_continuous -hmm data/asr_model/voxforge_es_sphinx.cd_ptm_4000 -lm data/asr_model/es-20k.lm -dict data/asr_model/es.dict -inmic yes 2> stt.log")
    flip evalStateT (DorsState Asleep) $ runConduit
        $  speechSource
        .| decodeUtf8C
        -- .| sink
        .| cleanUtterance
        .| handleKeywords
        .| emotionalAnalysis "data/emotional_lexicon_es.csv" "data/stopwords_es"
        -- -- .| conveyEmotion
        .| useUpEmotions

-- sink :: ConduitT Text Void Dors ()
-- sink = do
--     mval <- await
--     case mval of
--         Nothing -> sink
--         Just val -> liftIO (print $ "TRANS: " <> show val) >> sink


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
handleKeywords = awaitForever $ \utterance -> do
    case findKeyword utterance >>= keywordToCommand of
        Nothing  -> yield utterance
        Just cmd -> lift $ evalDorsCommand cmd
    where
        evalDorsCommand :: DorsCommand -> Dors ()
        evalDorsCommand cmd
            | cmd == WakeUp = do
                printCmd cmd
                (DorsState w) <- get
                case w of
                    Asleep -> do
                        liftIO wakeUpPhase1
                        put $ DorsState HalfAsleep
                    HalfAsleep -> do
                        liftIO wakeUpPhase2
                        put $ DorsState Awake
                    Awake -> pure ()

            | cmd == Sleep = printCmd cmd >> liftIO sleep

            | cmd == SayName = printCmd cmd >> liftIO sayName

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

        printCmd cmd = liftIO $ putStrLn $ "-- Running command: " <> show cmd

        keywords :: Set Text
        keywords = fromList
            $  sleepKeywords
            <> wakeUpKeywords
            <> sayNameKeywords

        sleepKeywords   = ["duerme", "duermete"]
        wakeUpKeywords  = ["despierta", "despiertate"]
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
            yield $ emotion utterance
            emotionalAnalysis emotional stopwords

conveyEmotion :: ConduitT E.Emotion Void Dors ()
conveyEmotion =
    awaitForever $ \emotion -> do
        liftIO $ putStrLn $ "-- Conveying emotion: " <> show emotion
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
