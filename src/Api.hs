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
import Data.Time    (UTCTime)
import Control.Monad.IO.Class
import GHC.Generics (Generic)
import Servant
import Data.Aeson
import Data.Aeson.Types
import Database.PostgreSQL.Simple
import Network.Wai
import Network.Wai.Handler.Warp

type EmotionAPI = "emotions" :> Get '[JSON] [EmotionRecord]

data EmotionRecord = EmotionRecord
    { emotion :: Text
    , at      :: UTCTime
    } deriving (Show, Generic)

instance ToJSON EmotionRecord

server :: Server EmotionAPI
server = do
    dbConn <- liftIO $ connectPostgreSQL "host=localhost port=5432 dbname=dors user=alx password=verde"
    emotions <- liftIO $ (query_ dbConn "select * from emotions":: IO [(Text, UTCTime)])
    pure $ (\(e, ts) -> EmotionRecord e ts) <$> emotions

emotionAPI :: Proxy EmotionAPI
emotionAPI = Proxy

app :: Application
app = serve emotionAPI server

runAPI :: IO ()
runAPI = run 8080 app
