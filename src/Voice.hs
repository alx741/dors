module Voice
    ( VoiceConfig(..)
    , defaultConfig
    , sayWithConfig
    , say
    , Noise(..)
    , makeNoise
    ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Data.Char          (toLower)
import Data.Text          (Text, unpack)
import System.Process     (callCommand)

data VoiceConfig = VoiceConfig
    { speed              :: Int
    , amplitude          :: Int
    , mainPitch          :: Int
    , wordGap            :: Int
    , overdrive          :: Int
    , postPitch          :: Int
    , mainEchoDecay      :: Float
    , secondaryEchoDecay :: Float
    } deriving (Show, Eq)

defaultConfig :: VoiceConfig
defaultConfig = VoiceConfig
    { speed              = 140
    , amplitude          = 100
    , mainPitch          = 80
    , wordGap            = 1
    , overdrive          = 10
    , postPitch          = 250
    , mainEchoDecay      = 0
    , secondaryEchoDecay = 0.7
    }

sayWithConfig :: VoiceConfig -> Text -> IO ThreadId
sayWithConfig cnf = forkIO . callCommand . buildCmd
    where
        buildCmd :: Text -> String
        buildCmd t
            =  "echo "
            <> unpack t
            <> " | espeak-ng --stdin -ves+f4"
            <> " -p " <> mainPitch' cnf'
            <> " -g " <> wordGap' cnf'
            <> " -a " <> amplitude' cnf'
            <> " -s " <> speed' cnf'
            <> " --stdout"
            <> " | play -"
            <> " overdrive " <> overdrive' cnf'
            <> " pitch " <> postPitch' cnf'
            -- <> " echo 0.8 0.88 60 " <> mainEchoDecay' cnf'
            -- <> " echo 0.8 0.7 6 " <> secondaryEchoDecay' cnf'
            where cnf' = voiceConfig2String cnf

-- | /Say/ using the 'defaultConfig'
say :: Text -> IO ThreadId
say = sayWithConfig defaultConfig

-- | Make a 'Noise'
-- takes sound effects directory
makeNoise :: FilePath -> Noise -> IO ThreadId
makeNoise fp = forkIO . callCommand . buildCmd
    where
        buildCmd :: Noise -> String
        buildCmd noise
            =  "play "
            <> fp <> "/"
            <> (toLower <$> show noise <> ".wav")
            <> " pitch 1000"
            <> " overdrive 10"
            <> " echo 0.8 0.88 60 0.2"
            <> " echo 0.8 0.7 6 0.7"

data Noise
    = WakeupMumbleUp
    | WakeupMumbleDown
    | Groan
    | Interjection
    | LittleLaughter
    | Whine
    | Ooh
    | Ah
    | Mmh
    deriving (Show, Eq)

data VoiceConfigString = VoiceConfigString
    { speed'              :: String
    , amplitude'          :: String
    , mainPitch'          :: String
    , wordGap'            :: String
    , overdrive'          :: String
    , postPitch'          :: String
    , mainEchoDecay'      :: String
    , secondaryEchoDecay' :: String
    } deriving (Show, Eq)

voiceConfig2String :: VoiceConfig -> VoiceConfigString
voiceConfig2String cnf = VoiceConfigString
    (show $ speed cnf)
    (show $ amplitude cnf)
    (show $ mainPitch cnf)
    (show $ wordGap cnf)
    (show $ overdrive cnf)
    (show $ postPitch cnf)
    (show $ mainEchoDecay cnf)
    (show $ secondaryEchoDecay cnf)
