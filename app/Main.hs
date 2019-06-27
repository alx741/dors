{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)

import Dors

main :: IO ()
main = getArgs >>= dors
