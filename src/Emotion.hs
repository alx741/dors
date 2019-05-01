{-# LANGUAGE FlexibleInstances #-}

module Emotion where

import           Control.Monad                    (mzero)
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy             as LBS
import           Data.Csv
import           Data.Foldable                    (fold)
import           Data.HashMap.Strict              as HM
import           Data.Maybe                       (fromJust, isJust)
import           Data.Text
import           Data.Vector                      as V hiding (head)
import           Numeric.Probability.Distribution as P
import           Prelude                          hiding (Word, words)

type Lexicon = HashMap Stem EmotionalDistribution
type EmotionalDistribution = P.T Double Emotion
type Stem = Text

instance Semigroup EmotionalDistribution where
    (<>) a b = fromFreqs $ fmap (\(e, p) -> (e, (p + unsafeLookup e a')/2)) b'
        where
            a' = decons a
            b' = decons b
            unsafeLookup :: Eq a => a -> [(a, b)] -> b
            unsafeLookup x = fromJust . Prelude.lookup x

instance Monoid EmotionalDistribution where
    mempty = P.enum (Prelude.replicate 11 0) (enumFrom Anticipation)

utteranceEmotionalDist :: Text -> Lexicon -> EmotionalDistribution
utteranceEmotionalDist t l = fold $ unMaybe $ flip wordEmotionalDist l <$> words t
    where
        unMaybe :: [Maybe a] -> [a]
        unMaybe = fmap fromJust . Prelude.filter isJust

wordEmotionalDist :: Text -> Lexicon -> Maybe EmotionalDistribution
wordEmotionalDist = HM.lookup . stem . strip

stem :: Text -> Text
stem = undefined

loadLexiconFile :: FilePath -> IO Lexicon
loadLexiconFile fp = decode HasHeader <$> LBS.readFile fp >>= either error (pure . vecToLexicon)
    where
         vecToLexicon :: Vector (Word Double) -> Lexicon
         vecToLexicon = HM.fromList . fmap (\w -> (wordStem w, emotionalDist w)) . V.toList

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
    { wordStem      :: !Text
    , emotionalDist :: EmotionalDistribution
    } deriving (Show)

instance Fractional prob => FromRecord (Word prob) where
    parseRecord v
        | V.length v == 15 = Word <$> v.! 1 <*> pure (P.enum intSlice emotions)
        | otherwise = mzero
        where
            emotions = enumFrom Anticipation
            intSlice = read . BS.unpack <$> (V.toList . V.slice 3 12) v
