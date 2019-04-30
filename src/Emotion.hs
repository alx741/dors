module Emotion where

import           Control.Monad                    (mzero)
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy             as LBS
import           Data.Csv
import           Data.HashMap.Strict              as HM
import           Data.Text
import           Data.Vector                      as V
import           Numeric.Probability.Distribution as P
import           Prelude                          hiding (Word)

type Lexicon = HashMap Stem EmotionalDistribution
type EmotionalDistribution = P.T Double Emotion
type Stem = Text

wordEmotionalDistribution :: Text -> P.T prob Emotion
wordEmotionalDistribution = undefined
-- wordEmotionalDistribution = stem

loadLexicon :: FilePath -> IO Lexicon
loadLexicon fp = decode HasHeader <$> LBS.readFile fp >>= either error (pure . vecToLexicon)
    where
         vecToLexicon :: Vector (Word Double) -> Lexicon
         vecToLexicon = HM.fromList . fmap (\w -> (stem w, emotionalDist w)) . V.toList

data Emotion
    = Anticipation
    | Joy
    | Trust
    | Fear
    | Sadness
    | Disgust
    | Anger
    | Surprise
    | Positive
    | Negative
    | None
    deriving (Show, Eq, Ord, Enum)

data Word prob = Word
    { stem          :: !Text
    , emotionalDist :: EmotionalDistribution
    } deriving (Show)

instance Fractional prob => FromRecord (Word prob) where
    parseRecord v
        | V.length v == 15 = Word <$> v.! 1 <*> pure (P.enum intSlice emotions)
        | otherwise = mzero
        where
            emotions = enumFrom Anticipation
            intSlice = read . BS.unpack <$> (V.toList . V.slice 3 12) v
