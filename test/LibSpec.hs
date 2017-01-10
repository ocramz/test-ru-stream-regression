{-# language FlexibleInstances, ScopedTypeVariables #-}
module LibSpec where

import qualified Data.Vector as V

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Lib 

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Numerical functions" $ do
    -- it "works" $ 
    --   True `shouldBe` True
    it "Inner product of two orthogonal vectors (R^3) is ~ 0" $
      nearZero (v2a <.> v2b) `shouldBe` True
    it "Inner product of two orthogonal vectors (R^67, after centering) is ~ 0" $
      nearZero (centerData v3a <.> centerData v3b) `shouldBe` True  
    it "Mean of an odd-symmetry sequence (R^23) is ~ 0" $
      nearZero (meanV v1) `shouldBe` True
    prop "Pearson R of a random vector (R^100) with itself is ~ 1" $
      \(x :: V.Vector Double) -> nearOne $ pearsonR x x
  describe "Constants" $ do
    it "Number of days == 183" $ nDaysTot `shouldBe` 183
    it " '', first period == 91" $ obsLen `shouldBe` 91
    it " '', second period == 92" $ obsLen2 `shouldBe` 92



-- | Arbitrary instances for QuickCheck

instance Arbitrary (V.Vector Double) where
  arbitrary = (V.fromList <$> vector 100) `suchThat` (nonZero . V.sum)




-- | Test data

v1, v2a, v2b, v3a, v3b :: V.Vector Double
v1 = V.fromList [-11 .. 11]

v2a = V.fromList [1, 0, 0]
v2b = V.fromList [0, 1, 0]

v3a = V.fromList [0..66]
v3b = V.fromList [66..0]
