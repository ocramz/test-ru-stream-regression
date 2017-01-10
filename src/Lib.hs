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




-- * Business logic

-- | Length (days) of observation periods
obsLen, obsLen2 :: Int
obsLen = 91
obsLen2 = 92

-- | Dataset file path
filePath :: String
filePath = "data/artist_data-short.csv"




parseDataset = do
  fcontents <- B.readFile filePath
  case P.parseOnly parseRows fcontents of Left e -> error e
                                          Right x ->
                                            return $ analyzeDataset x
      


-- | Linear correlation in time
analyzeDataset v = analyze <$> v  where
  t1_ = fi <$> V.enumFromTo 1 obsLen  -- time axis
  t2_ = fi <$> V.enumFromTo 1 obsLen2
  analyze (Row n d1 d2) = (n, pearsonR t1_ (fi <$> d1), pearsonR t2_ (fi <$> d2))
  







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


-- -- | Parser for the whole file
parseRows :: Parser B.ByteString (V.Vector (Row Int))
parseRows = V.fromList <$> P.sepBy parseRow P.endOfLine <* P.endOfInput






-- | Utilities

fi = fromIntegral

(<$$>) :: (Functor f1, Functor f) => (a -> b) -> f (f1 a) -> f (f1 b)
(<$$>) = fmap . fmap
    





-- -- test data

parseTest = P.parseOnly parseRow


t1 :: B.ByteString

t1 = "Shaana;4445;4425;4449;4335;4331;4315;4276;3825;3834;3960;3957;3543;3528;3616;3548;3559;3576;3519;3489;3488;3420;3337;3307;3288;3279;2711;3265;3260;3253;4617;4662;4662;4667;4654;4658;4844;4891;4887;4890;4895;4813;4799;4811;4965;5036;5062;5089;5094;5127;5127;5127;5184;4929;5060;5394;5391;5387;5379;5330;5330;5291;5412;5298;5282;4616;4630;4601;5078;5111;5061;5066;5066;5399;5398;5033;5049;5060;5418;5296;5201;5343;5338;5345;5378;5376;5519;5519;5518;5836;5615;5614;5614;5582;5601;5560;5552;5578;5624;5924;6035;5996;6006;6022;6299;6304;6391;6362;6376;6376;6301;6292;6304;6520;6518;6520;6533;6787;6780;6771;6843;7290;7284;7293;7256;7255;7247;7243;7194;7194;7086;7085;7217;7217;7230;7271;7220;7230;7320;7761;7742;7607;7579;7485;7480;7520;7531;7672;7654;7652;7652;7652;7571;7826;7796;7810;7923;7905;8140;8154;7825;7833;7830;7833;8005;8058;8118;8106;8075;8063;8254;8118;8092;8772;8711;8682;8702;8701;8285;8279;8279;8332;8351;8361"
