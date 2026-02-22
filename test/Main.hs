{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}

module Main (spec, main) where

import Data.Hashable (Hashable)
import Data.Maybe (isJust, isNothing)
import Dict
import SCDict
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (lookup)

instance (Arbitrary k, Arbitrary v, Hashable k) => Arbitrary (SCDict k v) where
  arbitrary = fromList <$> arbitrary

spec :: Spec
spec = do
  describe "empty" $ do
    it "has size 0" $
      size (empty :: SCDict Int Int) == 0
    it "lookup on empty returns Nothing" $
      property $ \k ->
        isNothing (lookup k (empty :: SCDict Int Int))

  describe "insert" $ do
    it "lookup after insert returns the value" $
      property $ \k v d ->
        lookup k (insert k v (d :: SCDict Int Int)) == Just v
    it "insert increases size by 1 for new key" $
      property $ \k v d ->
        isNothing (lookup k d) ==>
          size (insert k v (d :: SCDict Int Int)) == size d + 1
    it "insert overwrites existing key" $
      property $ \k v1 v2 d ->
        lookup k (insert k v2 (insert k v1 (d :: SCDict Int Int))) == Just v2
    it "insert does not affect other keys" $
      property $ \k1 k2 v d ->
        k1 /= k2 ==>
          lookup k1 (insert k2 v (d :: SCDict Int Int)) == lookup k1 d

  describe "delete" $ do
    it "lookup after delete returns Nothing" $
      property $ \k d ->
        isNothing (lookup k (delete k (d :: SCDict Int Int)))
    it "delete on missing key does not change size" $
      property $ \k d ->
        isNothing (lookup k d) ==>
          size (delete k (d :: SCDict Int Int)) == size d
    it "delete decreases size by 1 for existing key" $
      property $ \k (d :: SCDict Int Int) ->
        isJust (lookup k d) ==>
          size (delete k d) == size d - 1
    it "delete does not affect other keys" $
      property $ \k1 k2 d ->
        k1 /= k2 ==>
          lookup k1 (delete k2 (d :: SCDict Int Int)) == lookup k1 d

  describe "lookup" $ do
    it "returns Nothing for missing key" $
      property $ \k ->
        isNothing (lookup k (empty :: SCDict Int Int))
    it "returns Just value for existing key" $
      property $ \k v ->
        lookup k (insert k v (empty :: SCDict Int Int)) == Just v

  describe "mapValues" $ do
    it "applies function to all values" $
      property $ \k v d ->
        lookup k (mapValues (+ 1) (insert k v (d :: SCDict Int Int))) == Just (v + 1)
    it "does not change size" $
      property $ \d ->
        size (mapValues (+ 1) (d :: SCDict Int Int)) == size d
    it "does not change keys" $
      property $ \d ->
        let mapped = mapValues (+ 1) (d :: SCDict Int Int)
         in all (\k -> isJust (lookup k mapped)) (keys d)

  describe "filterDict" $ do
    it "removes entries that do not satisfy predicate" $
      property $ \d ->
        let filtered = filterDict (\_ v -> v > 0) (d :: SCDict Int Int)
         in all (\(_, v) -> v > 0) (toList filtered)
    it "keeps entries that satisfy predicate" $
      property $ \d ->
        let filtered = filterDict (\_ v -> v > 0) (d :: SCDict Int Int)
         in all (\(k, v) -> (v <= 0) || (lookup k filtered == Just v)) (toList d)
    it "filter with const True does not change dict" $
      property $ \d ->
        filterDict (\_ _ -> True) (d :: SCDict Int Int) == d
    it "filter with const False returns empty" $
      property $ \d ->
        size (filterDict (\_ _ -> False) (d :: SCDict Int Int)) == 0

  describe "foldLeft" $ do
    it "counts all entries" $
      property $ \d ->
        foldLeft (\acc _ -> acc + 1) 0 (d :: SCDict Int Int) == size d
    it "collects all keys" $
      property $ \d ->
        let ks = foldLeft (\acc (k, _) -> k : acc) [] (d :: SCDict Int Int)
         in length ks == size d

  describe "foldRight" $ do
    it "toList via foldRight has correct length" $
      property $ \d ->
        length (foldRight (:) [] (d :: SCDict Int Int)) == size d

  describe "size" $ do
    it "empty has size 0" $
      size (empty :: SCDict Int Int) `shouldBe` 0
    it "size after inserts" $
      property $ \kvs ->
        let d = fromList kvs :: SCDict Int Int
         in size d == length (toList d)

  describe "keys" $ do
    it "all keys are present in dict" $
      property $ \d ->
        all (\k -> isJust (lookup k d)) (keys (d :: SCDict Int Int))
    it "keys count equals size" $
      property $ \d ->
        length (keys (d :: SCDict Int Int)) == size d

  describe "values" $ do
    it "values count equals size" $
      property $ \d ->
        length (values (d :: SCDict Int Int)) == size d

  describe "toList" $ do
    it "toList length equals size" $
      property $ \d ->
        length (toList (d :: SCDict Int Int)) == size d
    it "all pairs from toList are in dict" $
      property $ \d ->
        all (\(k, v) -> lookup k (d :: SCDict Int Int) == Just v) (toList d)

  describe "fromList" $ do
    it "fromList then toList contains all unique keys" $
      property $ \kvs ->
        let d = fromList kvs :: SCDict Int Int
         in all (\(k, v) -> lookup k d == Just v) (toList d)
    it "later values overwrite earlier for duplicate keys" $
      property $ \k v1 v2 ->
        lookup k (fromList [(k, v1), (k, v2)] :: SCDict Int Int) == Just v2

  describe "union" $ do
    it "keys from both dicts are present" $
      property $ \d1 d2 ->
        let merged = union d1 (d2 :: SCDict Int Int)
         in all (\k -> isJust (lookup k merged)) (keys d1 ++ keys d2)
    it "d1 takes priority over d2 on conflict" $
      property $ \k v1 v2 ->
        let d1 = insert k v1 (empty :: SCDict Int Int)
            d2 = insert k v2 empty
         in lookup k (d1 `union` d2) == Just v1

  describe "Monoid axioms" $ do
    it "left identity" $
      property $ \dict ->
        (mempty <> dict :: SCDict String Int) == dict

    it "right identity" $
      property $ \dict ->
        (dict <> mempty :: SCDict String Int) == dict

    it "associativity" $
      property $ \a b c ->
        ((a <> b) <> c :: SCDict String Int) == (a <> (b <> c))

    it "union merges keys" $
      property $ \a b (k :: Int) ->
        let merged = a <> (b :: SCDict Int Int)
         in case (lookup k a, lookup k b) of
              (Just v, _) -> lookup k merged == Just v
              (Nothing, Just v) -> lookup k merged == Just v
              (Nothing, Nothing) -> isNothing (lookup k merged)

main :: IO ()
main = hspec spec
