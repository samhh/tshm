module UtilsSpec (spec) where

import           Data.Char           (isUpper)
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Prelude
import           Test.Hspec
import           Test.Hspec.Hedgehog (forAll, hedgehog, (===))
import           Utils

spec :: Spec
spec = describe "Utils" $ do
  let r = Range.linear 0 99
  let l = Gen.list r (Gen.int r)

  describe "arrangeBy" $ do
    it "returns identity on const predicate" $ hedgehog $ do
      xs <- forAll l
      arrangeBy (const . const $ True)  xs === xs
      arrangeBy (const . const $ False) xs === xs

    it "arranges per predicate" $ hedgehog $ do
      arrangeBy ((&&) `on` isUpper) "...Hello, World!" === "...HWello, orld!"

  describe "concatWhereBy" $ do
    it "returns identity on const false predicate" $ hedgehog $ do
      xs <- forAll l
      concatWhereBy const (const . const $ False) xs === xs

    it "returns head on const true predicate with const \"concat\"" $ hedgehog $ do
      xs@(x:_) <- forAll (Gen.filter (not . null) l)
      concatWhereBy const (const . const $ True) xs === [x]

    it "concats per predicate and provided function" $ hedgehog $ do
      concatWhereBy (+) ((&&) `on` (< (5 :: Int))) [2, 4, 1, 6, 3, 7, 2, 1, 1] === [6, 1, 6, 3, 7, 4]
