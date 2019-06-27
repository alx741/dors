{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Inquire where

import Data.Text as T

import Animation
import Voice

loadQuestions :: FilePath -> IO [Text]
loadQuestions fp = T.lines . pack <$> readFile fp

askRandomQuestion :: [Text] -> IO [Text]
askRandomQuestion [] = pure []
askRandomQuestion (q:qs) = goNeutral >> say q >> pure qs
