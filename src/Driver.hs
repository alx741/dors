module Driver
    ( robot
    , Command(..)
    , Emotion(..)
    , Position
    , position
    ) where

import Control.Monad              (unless)
import Data.Bits                  (shift, shiftR, (.&.), (.|.))
import Data.ByteString            as BS (pack)
import Data.ByteString.Char8      as BSC8 (pack)
import Data.Word                  (Word16, Word8)
import Prelude                    hiding (Left, Right)
import System.Hardware.Serialport

port :: String
port = "/dev/ttyUSB0"

robot :: Command -> IO ()
robot = sendCommand . renderCommand

data Command
    = SetEyes    Emotion
    | MoveHead   Position Position
    | SetEmotion Emotion
    | Shutdown
    deriving (Show)

newtype Position = Position Int deriving (Show)

data Emotion
    = Angry
    | Bored
    | Confused
    | Happy
    | Neutral
    | Sad
    | Sleepy
    | Smiley
    | Surprised
    | Suspicious
    deriving (Show)

type RawCommand = Word16

position :: Int -> Position
position x
    | x >= 0 && x <= 10 = Position x
    | otherwise = Position 0

sendCommand :: RawCommand -> IO ()
sendCommand cmd = do
    sp <-
        openSerial
            port
            defaultSerialSettings
            { commSpeed = CS9600
            }
    _ <- send sp $ BS.pack cmdSequence
    _ <- waitHardware sp
    return ()
    where
        cmdSequence :: [Word8]
        cmdSequence = fromIntegral <$> [0xFF .&. cmd, 0xFF .&. shiftR cmd 8]

        waitHardware :: SerialPort -> IO ()
        waitHardware sp = do
            char <- recv sp 1
            unless (BSC8.pack ">" == char) $ waitHardware sp


class RenderCommand a where
    renderCommand :: a -> RawCommand

instance RenderCommand Command where
    renderCommand (SetEyes a)    = 0x00 .|. shift (renderCommand a) 4
    renderCommand (MoveHead (Position a) (Position b))
        = 0x01
        .|. shift (fromIntegral a) 8
        .|. shift (fromIntegral b) 12
    renderCommand (SetEmotion a) = 0x02 .|. shift (renderCommand a) 4
    renderCommand Shutdown       = 0x03

instance RenderCommand Emotion where
    renderCommand Angry      = 0x01
    renderCommand Bored      = 0x02
    renderCommand Confused   = 0x03
    renderCommand Happy      = 0x04
    renderCommand Neutral    = 0x05
    renderCommand Sad        = 0x06
    renderCommand Sleepy     = 0x07
    renderCommand Smiley     = 0x08
    renderCommand Surprised  = 0x09
    renderCommand Suspicious = 0x0A
