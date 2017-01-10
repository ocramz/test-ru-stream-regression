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
  describe "Numerical functions" $ do
    -- it "works" $ 
    --   True `shouldBe` True
    prop "Pearson R of a random vector with itself is ~ 1" $
      \(x :: V.Vector Double) -> nearOne $ pearsonR x x



-- | Arbitrary instances for QuickCheck

instance Arbitrary (V.Vector Double) where
  arbitrary = (V.fromList <$> vector 100) `suchThat` (nonZero . V.sum)


