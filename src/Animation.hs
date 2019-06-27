{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Animation where

import Control.Concurrent (threadDelay)
import Driver
import Voice              (Noise (..), makeNoise, say)

-- | Wait n milliseconds
wait :: Int -> IO ()
wait = threadDelay . (* 1000)

noiseDir = "./data/sound"
makeNoise' = makeNoise noiseDir

goNeutral :: IO ()
goNeutral = do
    setEmotion Neutral
    wait 1000

wakeUpPhase1 :: IO ()
wakeUpPhase1 = do
    setEyes Suspicious
    wait 2000
    makeNoise' WakeupMumbleUp
    setHead (position 5) (position 5)
    wait 1000
    setEyes Bored
    wait 2000
    setEyes Suspicious
    wait 2000
    makeNoise' WakeupMumbleDown
    setHead (position 0) (position 5)
    wait 1000
    robot Shutdown

wakeUpPhase2 :: IO ()
wakeUpPhase2 = do
    setEyes Suspicious
    wait 2000
    makeNoise' WakeupMumbleUp
    setHead (position 5) (position 5)
    wait 2000
    setEyes Angry
    setHead (position 5) (position 0)
    makeNoise' Groan
    wait 1000
    setHead (position 5) (position 10)
    wait 1000
    setHead (position 5) (position 5)
    wait 1000
    setEyes Bored
    wait 1000
    setEyes Neutral

sleep :: IO ()
sleep = do
    setEmotion Neutral
    wait 2000
    setEyes Bored
    wait 2000
    setEyes Suspicious
    wait 1000
    makeNoise' WakeupMumbleDown
    setHead (position 0) (position 5)
    wait 1000
    robot Shutdown

sayName :: IO ()
sayName = do
    setEmotion Neutral
    wait 1000
    setEmotion Smiley
    say "me llamo dors"
    wait 1500
    makeNoise' LittleLaughter
    wait 2000
    setEmotion Neutral

sayHello :: IO ()
sayHello = do
    setEmotion Happy
    say "hola"
    wait 1000
    setEmotion Neutral
