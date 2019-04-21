module Driver where

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
