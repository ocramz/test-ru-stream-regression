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
spec =
  describe "Lib" $ do
    it "works" $ 
      True `shouldBe` True
    -- it "Pearson R of a vector with itself is ~ 1" $
    --   nearOne (pearsonR v0 v0) `shouldBe` True
    prop "Pearson R of a vector with itself is ~ 1" $ \(x :: V.Vector Double) -> 
      nearOne $ pearsonR x x



-- | Arbitrary instances for QuickCheck

instance Arbitrary (V.Vector Double) where
  arbitrary = (V.fromList <$> vector 10) `suchThat` (\x -> V.sum x > 0)



-- | test data

v0 :: V.Vector Double
v0 = V.fromList [1,2,3]
