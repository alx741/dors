module Voice where

import Data.Text (Text, unpack)

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

defaultConfig :: VoiceConfig
defaultConfig = VoiceConfig
    { speed              = 140
    , amplitude          = 100
    , mainPitch          = 80
    , wordGap            = 1
    , overdrive          = 1
    , postPitch          = 300
    , mainEchoDecay      = 0.2
    , secondaryEchoDecay = 0.7
    }

sayWithConfig :: VoiceConfig -> Text -> IO ()
sayWithConfig cnf t = undefined

buildCmd :: VoiceConfig -> Text ->  String
buildCmd cnf t
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
    <> " echo 0.8 0.88 60 " <> mainEchoDecay' cnf'
    <> " echo 0.8 0.7 6 " <> secondaryEchoDecay' cnf'
    where cnf' = voiceConfig2String cnf
