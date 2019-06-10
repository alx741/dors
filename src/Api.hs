{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Api where

import Data.Text
import Control.Monad.IO.Class
import GHC.Generics (Generic)
import Servant
import Data.Aeson
import Database.PostgreSQL.Simple
import Network.Wai.Handler.Warp

type EmotionAPI
    = "emotions" :> Get '[JSON] [EmotionRecord]
    :<|> Raw

data EmotionRecord = EmotionRecord
    { emotion :: Text
    , count :: Int
    } deriving (Show, Generic)

instance ToJSON EmotionRecord

server :: Server EmotionAPI
server = serveEmotions :<|> serveDirectoryFileServer "ui"
    where
        serveEmotions = do
            dbConn <- liftIO $ connectPostgreSQL "host=localhost port=5432 dbname=dors user=alx password=verde"
            emotions <- liftIO $ (query_ dbConn "select emotion, count(emotion) from emotions group by emotion":: IO [(Text, Int)])
            pure $ (\(e, cnt) -> EmotionRecord e cnt) <$> emotions

emotionAPI :: Proxy EmotionAPI
emotionAPI = Proxy

app :: Application
app = serve emotionAPI server

runAPI :: IO ()
runAPI = run 3000 app
