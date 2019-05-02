module Text.Mining.StopWords where

import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe           (fromJust)
import           Data.Set             (Set, fromList, member)
import           Data.Text            as T (Text, toLower, unwords, words)

import Text.Clean (removeAccents)

type Lexicon = Set Text

removeStopWordsOnLexicon :: Lexicon -> Text -> Text
removeStopWordsOnLexicon l
    = T.unwords
    . filter (\w -> not $ (toLower . removeAccents) w `member` l)
    . T.words

loadLexiconFile :: FilePath -> IO Lexicon
loadLexiconFile fp
    = fromList
    . fmap (toLower . removeAccents)
    . fromJust
    . decode <$> LBS.readFile fp
