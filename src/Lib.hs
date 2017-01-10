{-# language OverloadedStrings #-}

module Lib where

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as VA

-- import System.IO
-- import Data.Text

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B


import Data.Attoparsec.Internal.Types (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P (decimal, signed, scientific, digit, number, rational, anyChar, letter_ascii, char, char8, endOfLine, endOfInput, isDigit, isDigit_w8, isEndOfLine, isHorizontalSpace, parseOnly)
import qualified Data.Attoparsec.ByteString.Lazy as P

import Data.Char
import Data.Either

filePath :: String
filePath = "data/artist_data.csv"



data Row = Row { artistName :: String,
                 dailyStreams :: V.Vector Int } deriving (Eq, Show)


parse1 = do
  fcontents <- BL.readFile filePath
  return $ P.parse (P.many' parseRow <* P.endOfInput) fcontents


spacer = P.char ';'

parseRow = do
  n <- P.many' P.letter_ascii <* spacer
  d <- P.sepBy P.decimal spacer
  _ <- P.endOfLine
  return $ Row n (V.fromList d)





--

ttt = "Menandez;5749;5782;5765;5703;5696;5730;5731;5658;5653;5622;5621;5711;5731;5562;5568;5590;5511;5452;5433;5436;5457;5191;5055;5085;5082;5452;5425;5414;5418;4876;4830;4830;4829;4829;4821;4904;4896;4892;4874;4886;4873;4869;4874;4547;4375;4461;4695;4660;4652;4651;4642;4626;5001;4998;5518;5510;5496;5494;5419;5419;5391;5212;5104;5088;5802;5885;5952;5972;5987;6072;6071;6070;6596;6594;6921;6943;6915;7011;7042;7167;7301;7291;7271;7312;7310;7224;7219;7219;7010;7030;7030;7031;7041;6925;6795;6800;6854;6787;6283;6522;6348;6316;6307;5990;5975;6354;6346;6346;6346;6528;6507;6816;7121;7121;7120;7112;7245;7243;7239;7353;7603;7599;7614;7598;7598;7595;7595;7558;7558;7734;7734;7577;7576;7386;7416;7407;7612;7619;7606;7600;7364;7393;6618;6617;6465;6446;6593;6604;6580;6578;6577;6084;5982;5991;5989;5909;5924;6176;6184;7205;7235;7291;7306;7159;7194;7160;7150;7188;7176;8086;7939;7931;8277;8618;8594;8583;8582;8314;8314;8312;8198;8183;8184\n" :: B.ByteString
