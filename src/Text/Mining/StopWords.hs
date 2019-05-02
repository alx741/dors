module Text.Mining.StopWords where

import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe           (fromJust)
import           Data.Set
import           Data.Text

import Text.Clean (removeAccents)

type Lexicon = Set Text

-- TODO: Implement
removeStopWordsOnLexicon :: Lexicon -> Text -> Text
removeStopWordsOnLexicon = undefined

loadLexiconFile :: FilePath -> IO Lexicon
loadLexiconFile fp
    = fromList
    . fmap removeAccents
    . fromJust
    . decode <$> LBS.readFile fp
