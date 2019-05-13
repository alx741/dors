module Voice where

import Data.Text (Text)

data VoiceConfig = VoiceConfig
    { speed              :: Int
    , amplitude          :: Int
    , mainPitch          :: Int
    , wordGap            :: Int
    , overdrie           :: Int
    , postPitch          :: Int
    , mainEchoDecay      :: Float
    , secondaryEchoDecay :: Float
    } deriving (Show, Eq)

sayWithConfig :: VoiceConfig -> Text -> IO ()
sayWithConfig cnf = undefined

defaultConfig :: VoiceConfig
defaultConfig = VoiceConfig
    { speed              = 140
    , amplitude          = 100
    , mainPitch          = 80
    , wordGap            = 1
    , overdrie           = 1
    , postPitch          = 300
    , mainEchoDecay      = 0.2
    , secondaryEchoDecay = 0.7
    }
