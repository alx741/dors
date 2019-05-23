module Driver
    ( robot
    , setHead
    , setEyes
    , setEmotion
    , Command(..)
    , PhyEmotion(..)
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
robot = sendCommand . serializeCommand

setHead :: Position -> Position -> IO ()
setHead p = robot . MoveHead p

setEyes :: PhyEmotion -> IO ()
setEyes = robot . SetEyes

setEmotion :: PhyEmotion -> IO ()
setEmotion = robot . SetEmotion

data Command
    = SetEyes    PhyEmotion
    | MoveHead   Position Position
    | SetEmotion PhyEmotion
    | Shutdown
    deriving (Show)

newtype Position = Position Int deriving (Show)

data PhyEmotion
    = Angry
    | Bored
    | Confused
    | Happy
    | Love
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


class SerializeCommand a where
    serializeCommand :: a -> RawCommand

instance SerializeCommand Command where
    serializeCommand (SetEyes a)    = 0x00 .|. shift (serializeCommand a) 4
    serializeCommand (MoveHead (Position a) (Position b))
        = 0x01
        .|. shift (fromIntegral a) 8
        .|. shift (fromIntegral b) 12
    serializeCommand (SetEmotion a) = 0x02 .|. shift (serializeCommand a) 4
    serializeCommand Shutdown       = 0x03

instance SerializeCommand PhyEmotion where
    serializeCommand Angry      = 0x01
    serializeCommand Bored      = 0x02
    serializeCommand Confused   = 0x03
    serializeCommand Happy      = 0x04
    serializeCommand Neutral    = 0x05
    serializeCommand Sad        = 0x06
    serializeCommand Sleepy     = 0x07
    serializeCommand Smiley     = 0x08
    serializeCommand Surprised  = 0x09
    serializeCommand Suspicious = 0x0A
