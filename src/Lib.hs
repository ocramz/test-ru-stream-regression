{-# language OverloadedStrings #-}
module Lib where

import Control.Applicative
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as VA

-- import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

import Data.Attoparsec.Internal.Types (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P (decimal, letter_ascii, char, endOfLine, endOfInput, parseOnly, space, count)
import qualified Data.Attoparsec.ByteString.Lazy as P

import Data.Ord



-- * Business logic

-- | Length (days) of observation periods
obsLen, obsLen2 :: Int
obsLen = 91
obsLen2 = 92

-- | Dataset file path
filePath :: String
filePath = "data/artist_data.csv"

-- | Size of rankings (default, can be overridden from command line)
topN :: Int
topN = 3


-- | The IO part of the program : read data, parse it, process it, display results
processDataset :: Int -> IO ()
processDataset topn = do
  fcontents <- B.readFile filePath
  case P.parseOnly parseRows fcontents of
    Left e -> error e
    Right x -> do
     let dat = sortResults $ analyzeDataset x  -- NB: sorted in descending order
         n = V.length dat
         rankHi = V.take topn dat
         rankLo = V.drop (n - topn) dat
     putStrLn $ unwords ["Rankings (high):", show rankHi]
     putStrLn $ unwords ["Rankings (low):", show rankLo]  
     putStrLn $ unwords ["Inter-period correlation :", show (correlatePeriods dat)]



-- | Linear correlation in time of stream count for both observation periods
analyzeDataset :: V.Vector (Row Int) -> V.Vector (String, Double, Double)
analyzeDataset v = V.map analyze v  where
  analyze row = (artistName row,
                 pearsonR t1_ (fi <$> dailyStreams1 row),
                 pearsonR t2_ (fi <$> dailyStreams2 row))
  t1_ = fi <$> V.enumFromTo 1 obsLen  -- time axis
  t2_ = fi <$> V.enumFromTo 1 obsLen2
  
-- | Sort rows in _descending_ order according to time linear regression w.r.t.
-- second period only
sortResults :: Ord a => V.Vector (t, t1, a) -> V.Vector (t, t1, a)
sortResults v = V.modify (VA.sortBy fs) v where
  fs (_, _, c2a) (_, _, c2b) = compare (Down c2a) (Down c2b)   -- comparison function

-- | Pearson correlation between the two vectors of linear trends, i.e. how well the
-- artist trends for the first period correlates linearly with the artist trends for
-- the second period
correlatePeriods :: Floating b => V.Vector (a, b, b) -> b
correlatePeriods v = pearsonR c1_ c2_ where
           (_, c1_, c2_) = V.unzip3 v

  




-- * Numerical functions

-- | Pearson linear correlation coefficient
pearsonR :: Floating a => V.Vector a -> V.Vector a -> a
pearsonR x y = (xm <.> ym) / (sx * sy) where
  xm = centerData x
  ym = centerData y
  sx = sqrt (xm <.> xm)
  sy = sqrt (ym <.> ym)
  

-- | Sample mean
meanV :: Fractional a => V.Vector a -> a
meanV v = V.sum v / fromIntegral (V.length v)

-- | Subtract the sample mean from a sample
centerData :: Fractional a => V.Vector a -> V.Vector a
centerData v = subtract (meanV v) <$> v

-- | Inner product
(<.>) :: Num a => V.Vector a -> V.Vector a -> a
a <.> b = sum (V.zipWith (*) a b)



-- | Near-zero test that depends on the numerical precision used

class Num e => Epsilon e where
  nearZero :: e -> Bool

instance Epsilon Double where nearZero x = x <= 1e-12

nearOne :: Epsilon e => e -> Bool
nearOne x = nearZero (1 - x) 

nonZero :: Epsilon e => e -> Bool
nonZero = not . nearZero








-- * Parsing-related functions

data Row a = Row { artistName :: String,
                   dailyStreams1 :: V.Vector a,  -- ^ 1st obs.period
                   dailyStreams2 :: V.Vector a   -- ^ 2nd obs.period
                 } deriving (Eq, Show)


-- | Parser for a single row
parseRow :: Parser B.ByteString (Row Int)
parseRow = do
  n <- parseName <* spacer
  d1 <- P.count obsLen (P.decimal <* spacer)
  d2 <- P.sepBy P.decimal spacer
  return $ Row n (V.fromList d1) (V.fromList d2)
  where
    spacer = P.char ';'
    parseName = P.many' (P.letter_ascii <|> P.space <|> cc1 <|> cc2) where
      cc1 = P.char '\''  -- corner cases in artists' names
      cc2 = P.char '-'


-- | Parser for the whole file
parseRows :: Parser B.ByteString (V.Vector (Row Int))
parseRows = V.fromList <$> P.sepBy parseRow P.endOfLine <* P.endOfInput






-- | Utilities

fi :: Int -> Double
fi = fromIntegral


