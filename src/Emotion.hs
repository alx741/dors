module Emotion where

import           Control.Monad                    (mzero)
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy             as LBS
import           Data.Csv
import           Data.Text
import           Data.Vector                      as V
import           Numeric.Probability.Distribution as P
import           Prelude                          hiding (Word)

type Lexicon prob = Vector (Word prob)

loadLexicon :: Fractional prob => FilePath -> IO (Lexicon prob)
loadLexicon fp = LBS.readFile fp >>= pure . decode HasHeader >>= either error pure

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
    { stem                :: Text
    , emotionDistribution :: P.T prob Emotion
    } deriving (Show)

instance Fractional prob => FromRecord (Word prob) where
    parseRecord v
        | V.length v == 15 = Word <$> v.! 1 <*> pure (P.enum intSlice emotions)
        | otherwise = mzero
        where
            emotions = enumFrom Anticipation
            intSlice = read . BS.unpack <$> (V.toList . V.slice 3 12) v
