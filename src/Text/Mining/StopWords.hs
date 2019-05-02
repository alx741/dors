module Text.Mining.StopWords where

import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe           (fromJust)
import           Data.Set             (Set, fromList, member)
import           Data.Text            as T (Text, unwords, words)

import Text.Clean (removeAccents)

type Lexicon = Set Text

removeStopWordsOnLexicon :: Lexicon -> Text -> Text
removeStopWordsOnLexicon l = T.unwords . filter (`member` l) . T.words

loadLexiconFile :: FilePath -> IO Lexicon
loadLexiconFile fp
    = fromList
    . fmap removeAccents
    . fromJust
    . decode <$> LBS.readFile fp
