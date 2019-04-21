module Driver where

import Control.Monad              (unless)
import Data.ByteString            as BS (singleton)
import Data.ByteString.Char8      as BSC8 (pack)
import Data.Word                  (Word8)
import System.Hardware.Serialport

port :: String
port = "/dev/ttyUSB0"

robot :: Command -> IO ()
robot = undefined

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

hardwareExecute :: Command -> IO ()
hardwareExecute = sendCommand . renderCommand

renderCommand :: Command -> RawCommand
renderCommand = undefined

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
