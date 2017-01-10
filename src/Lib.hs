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


parseDataset = do
  fcontents <- BL.readFile filePath
  return $ P.parse parseRows fcontents


parseRow :: Parser B.ByteString Row
parseRow = do
  n <- P.many' P.letter_ascii <* spacer
  d <- P.sepBy P.decimal spacer
  return $ Row n (V.fromList d) where
    spacer = P.char ';'


-- parseRows = P.many' (parseRow <* P.endOfLine) <* P.endOfInput

parseRows :: Parser B.ByteString (V.Vector Row)
parseRows = V.fromList <$> P.sepBy parseRow P.endOfLine <* P.endOfInput



--

parseTest = P.parseOnly parseRows t2

t1, t2 :: B.ByteString

t1 = "Menandez;5749;5782;5765;5703;5696;5730;5731;5658;5653;5622;5621;5711;5731;5562;5568;5590;5511;5452;5433;5436;5457;5191;5055;5085;5082;5452;5425;5414;5418;4876;4830;4830;4829;4829;4821;4904;4896;4892;4874;4886;4873;4869;4874;4547;4375;4461;4695;4660;4652;4651;4642;4626;5001;4998;5518;5510;5496;5494;5419;5419;5391;5212;5104;5088;5802;5885;5952;5972;5987;6072;6071;6070;6596;6594;6921;6943;6915;7011;7042;7167;7301;7291;7271;7312;7310;7224;7219;7219;7010;7030;7030;7031;7041;6925;6795;6800;6854;6787;6283;6522;6348;6316;6307;5990;5975;6354;6346;6346;6346;6528;6507;6816;7121;7121;7120;7112;7245;7243;7239;7353;7603;7599;7614;7598;7598;7595;7595;7558;7558;7734;7734;7577;7576;7386;7416;7407;7612;7619;7606;7600;7364;7393;6618;6617;6465;6446;6593;6604;6580;6578;6577;6084;5982;5991;5989;5909;5924;6176;6184;7205;7235;7291;7306;7159;7194;7160;7150;7188;7176;8086;7939;7931;8277;8618;8594;8583;8582;8314;8314;8312;8198;8183;8184" 

t2 = "Menandez;5749;5782;5765;5703;5696;5730;5731;5658;5653;5622;5621;5711;5731;5562;5568;5590;5511;5452;5433;5436;5457;5191;5055;5085;5082;5452;5425;5414;5418;4876;4830;4830;4829;4829;4821;4904;4896;4892;4874;4886;4873;4869;4874;4547;4375;4461;4695;4660;4652;4651;4642;4626;5001;4998;5518;5510;5496;5494;5419;5419;5391;5212;5104;5088;5802;5885;5952;5972;5987;6072;6071;6070;6596;6594;6921;6943;6915;7011;7042;7167;7301;7291;7271;7312;7310;7224;7219;7219;7010;7030;7030;7031;7041;6925;6795;6800;6854;6787;6283;6522;6348;6316;6307;5990;5975;6354;6346;6346;6346;6528;6507;6816;7121;7121;7120;7112;7245;7243;7239;7353;7603;7599;7614;7598;7598;7595;7595;7558;7558;7734;7734;7577;7576;7386;7416;7407;7612;7619;7606;7600;7364;7393;6618;6617;6465;6446;6593;6604;6580;6578;6577;6084;5982;5991;5989;5909;5924;6176;6184;7205;7235;7291;7306;7159;7194;7160;7150;7188;7176;8086;7939;7931;8277;8618;8594;8583;8582;8314;8314;8312;8198;8183;8184\nWilliam;4242;4245;4216;4216;4207;4196;4169;3441;3445;3665;3667;3421;3424;3499;3645;3669;3598;3688;3670;3667;3673;3866;4109;4133;4133;3840;3921;3900;3902;3980;3927;3926;3926;3958;3968;3831;3874;3916;3924;3883;4068;4074;4089;4654;4828;4832;4993;4990;5029;5029;5024;5111;5137;5130;5138;5144;5149;5158;5167;5165;5165;5266;5302;5299;4311;3817;3658;3473;3494;3412;3413;3413;2695;2696;3073;3110;3114;3762;3863;3705;3813;3819;3827;4004;4003;4170;4175;4174;4483;4539;4539;4537;4571;4604;4671;4628;4641;4650;4103;4191;4272;4280;4218;4020;4019;3203;3249;3218;3217;3173;3190;3124;3284;3281;3281;3252;2941;2945;2948;2952;3165;3161;3178;3170;3170;3168;3171;3166;3165;3082;3082;3077;3077;3122;3118;3147;3126;3127;3369;3378;3400;3420;3375;3375;3273;3229;3223;3213;3188;3187;3187;3284;3265;3305;3297;3301;3273;3036;3007;2750;2785;2700;2724;2921;2561;2586;2586;2590;2601;2316;2396;2379;1986;1938;1884;1881;1881;1805;1805;1805;1832;1831;1847\nThaine;3878;3885;3852;3866;3868;3838;3810;4062;4077;3928;3929;3800;3891;4240;4183;4222;4225;4210;4236;4224;4267;4171;4209;4252;4252;3903;4026;4021;4015;3102;3116;3116;3114;3099;3081;3048;2964;2944;2945;2966;2908;2923;2916;3552;3308;3246;2793;2867;2912;2911;2909;2914;3168;3121;2643;2627;2626;2636;2632;2631;2654;2567;2646;2638;2547;2211;2118;1452;1456;1704;1705;1705;1988;1987;2172;2203;2162;1577;1625;1601;1487;1489;1478;1300;1300;1353;1348;1347;1147;761;760;761;791;742;629;621;838;949;1755;1922;2036;2009;2002;1641;1642;1788;1836;1810;1810;1888;1869;1685;2033;2034;2035;2032;2058;2045;2046;2099;1879;1870;1907;1891;1891;1889;1886;1907;1906;1844;1844;1876;1875;1995;1920;1964;1821;1819;1660;1687;1654;1645;1904;1904;1894;1907;1979;1972;1984;1982;1982;2477;2414;2380;2375;2477;2502;2278;2285;1850;1897;1885;1921;2006;1744;1756;1749;1695;1684;2016;2051;2066;2027;2462;2446;2424;2421;2036;2031;2032;1941;1935;1944"
