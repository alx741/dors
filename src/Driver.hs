module Driver
    ( robot
    , Command(..)
    , Emotion(..)
    , Direction(..)
    ) where

import Control.Monad              (unless)
import Data.Bits
import Data.ByteString            as BS (singleton)
import Data.ByteString.Char8      as BSC8 (pack)
import Data.Word                  (Word8)
import Prelude                    hiding (Left, Right)
import System.Hardware.Serialport

port :: String
port = "/dev/ttyUSB0"

robot :: Command -> IO ()
robot = sendCommand . renderCommand

data Command
    = SetEyes    Emotion
    | MoveHead   Direction
    | SetEmotion Emotion
    deriving (Show)

data Direction
    = Up
    | Down
    | Left
    | Right
    deriving (Show)

data Emotion
    = Neutral
    | Happy
    | Sad
    | Surprised
    | Bored
    deriving (Show)

type RawCommand = Word8

sendCommand :: RawCommand -> IO ()
sendCommand cmd = do
    sp <-
        openSerial
            port
            defaultSerialSettings
            { commSpeed = CS9600
            }
    _ <- send sp $ BS.singleton cmd
    _ <- waitHardware sp
    return ()
    where
        waitHardware :: SerialPort -> IO ()
        waitHardware sp = do
            char <- recv sp 1
            unless (BSC8.pack ">" == char) $ waitHardware sp


class RenderCommand a where
    renderCommand :: a -> RawCommand

instance RenderCommand Command where
    renderCommand (SetEyes a)    = 0x00 .|. shift (renderCommand a) 4
    renderCommand (MoveHead a)   = 0x01 .|. shift (renderCommand a) 4
    renderCommand (SetEmotion a) = 0x02 .|. shift (renderCommand a) 4

instance RenderCommand Emotion where
    renderCommand Neutral   = 0x01
    renderCommand Happy     = 0x02
    renderCommand Sad       = 0x03
    renderCommand Surprised = 0x04
    renderCommand Bored     = 0x05

instance RenderCommand Direction where
    renderCommand Up    = 0x01
    renderCommand Down  = 0x02
    renderCommand Left  = 0x03
    renderCommand Right = 0x04
