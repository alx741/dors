module Animation where

import Control.Concurrent (threadDelay)

import Driver

-- | Wait n milliseconds
wait :: Int -> IO()
wait = threadDelay . (\x -> x * 1000)

wakeUpPhase1 :: IO()
wakeUpPhase1 = do
    setEyes Suspicious
    wait 3000
    setHead (position 5) (position 5)
    wait 1000
    setEyes Bored
    wait 3000
    setEyes Suspicious
    wait 2000
    setHead (position 0) (position 5)
    wait 1000
    robot Shutdown

wakeUpPhase2 :: IO()
wakeUpPhase2 = do
    setEyes Suspicious
    wait 3000
    setHead (position 5) (position 5)
    wait 3000
    setEyes Suspicious
    wait 1000
    setEyes Angry
    wait 1000
    setHead (position 5) (position 0)
    wait 2000
    setHead (position 5) (position 10)
    wait 2000
    setHead (position 5) (position 5)
    wait 2000
    setEyes Bored
    wait 2000
    setEyes Neutral

sleep :: IO()
sleep = do
    setHead (position 5) (position 5)
    wait 3000
    setEyes Bored
    wait 3000
    setEyes Suspicious
    wait 3000
    setHead (position 0) (position 5)
    wait 3000
    robot Shutdown
