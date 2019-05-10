module Main where

import Prelude hiding (Left, Right)

import Driver

main :: IO ()
main = do
    printEmotions
    loop
    where
        loop = do
            c <- getChar
            case c of
                'a' -> robot (SetEyes Angry)      >> loop
                's' -> robot (SetEyes Bored)      >> loop
                'd' -> robot (SetEyes Confused)   >> loop
                'f' -> robot (SetEyes Happy)      >> loop
                'g' -> robot (SetEyes Neutral)    >> loop
                'h' -> robot (SetEyes Sad)        >> loop
                'j' -> robot (SetEyes Sleepy)     >> loop
                'k' -> robot (SetEyes Smiley)     >> loop
                'l' -> robot (SetEyes Surprised)  >> loop
                ';' -> robot (SetEyes Suspicious) >> loop

                'q' -> robot (MoveHead Up)     >> loop
                'w' -> robot (MoveHead Down)     >> loop
                'e' -> robot (MoveHead Left)  >> loop
                't' -> robot (MoveHead Right) >> loop
                _   -> loop

printEmotions :: IO ()
printEmotions = do
    putStrLn "Angry   [a]   Bored [s]   Confused  [d]"
    putStrLn "Neutral [g]   Sad   [h]   Sleepy    [j]"
    putStrLn "Smiley  [k]   Happy [f]   Surprised [l]   Suspicious [;]"
