module SCDict
  ( SCDict,
  )
where

import Data.Hashable (Hashable, hash)
import qualified Data.List as List
import Dict
import Prelude hiding (lookup)

type Bucket k v = [(k, v)]

data SCDict k v = SCDict
  { numBuckets :: Int,
    buckets :: [Bucket k v]
  }

-- Internal helper functions
defaultSize :: Int
defaultSize = 16

bucketIndex :: (Hashable k) => k -> Int -> Int
bucketIndex key bucketsCount =
  abs (hash key) `mod` bucketsCount

updateAt :: Int -> a -> [a] -> [a]
updateAt _ _ [] = []
updateAt 0 new (_ : xs) = new : xs
updateAt i new (x : xs) = x : updateAt (i - 1) new xs

insertInBucket :: (Eq k) => k -> v -> Bucket k v -> Bucket k v
insertInBucket key val [] = [(key, val)]
insertInBucket key val ((k, v) : xs)
  | key == k = (key, val) : xs
  | otherwise = (k, v) : insertInBucket key val xs

deleteFromBucket :: (Eq k) => k -> Bucket k v -> Bucket k v
deleteFromBucket _ [] = []
deleteFromBucket key ((k, v) : xs)
  | key == k = xs
  | otherwise = (k, v) : deleteFromBucket key xs

foldLeftBucket ::
  (acc -> (k, v) -> acc) ->
  acc ->
  Bucket k v ->
  acc
foldLeftBucket _ acc [] = acc
foldLeftBucket f acc (x : xs) =
  foldLeftBucket f (f acc x) xs

foldRightBucket ::
  ((k, v) -> acc -> acc) ->
  acc ->
  Bucket k v ->
  acc
foldRightBucket _ acc [] = acc
foldRightBucket f acc (x : xs) =
  f x (foldRightBucket f acc xs)

-- Instance Dict
instance Dict SCDict where
  empty =
    SCDict
      { numBuckets = defaultSize,
        buckets = replicate defaultSize []
      }

  insert key val dict =
    let idx = bucketIndex key (numBuckets dict)
        oldBucket = buckets dict !! idx
        newBucket = insertInBucket key val oldBucket
        newBuckets = updateAt idx newBucket (buckets dict)
     in dict {buckets = newBuckets}

  delete key dict =
    let idx = bucketIndex key (numBuckets dict)
        oldBucket = buckets dict !! idx
        newBucket = deleteFromBucket key oldBucket
        newBuckets = updateAt idx newBucket (buckets dict)
     in dict {buckets = newBuckets}

  lookup key dict =
    let idx = bucketIndex key (numBuckets dict)
        bucket = buckets dict !! idx
     in List.lookup key bucket

  mapValues f dict =
    let newBuckets =
          map
            (map (\(k, v) -> (k, f v)))
            (buckets dict)
     in dict {buckets = newBuckets}

  filterDict p dict =
    let newBuckets =
          map
            (filter (\(k, v) -> p k v))
            (buckets dict)
     in dict {buckets = newBuckets}

  foldLeft f acc dict =
    go acc (buckets dict)
    where
      go a [] = a
      go a (b : rest) =
        go (foldLeftBucket f a b) rest

  foldRight f acc dict =
    go (buckets dict)
    where
      go [] = acc
      go (b : rest) =
        foldRightBucket f (go rest) b

  size =
    foldRight (\_ acc -> acc + 1) 0

  keys =
    foldLeft (\acc (k, _) -> k : acc) []

  values =
    foldLeft (\acc (_, v) -> v : acc) []

  toList =
    foldRight (:) []

  fromList =
    foldl (\d (k, v) -> insert k v d) empty

  union d1 d2 =
    foldLeft (\acc (k, v) -> insert k v acc) d2 d1

-- Eq instance
instance (Eq k, Eq v, Hashable k) => Eq (SCDict k v) where
  d1 == d2 =
    size d1 == size d2
      && all
        (\(k, v) -> lookup k d2 == Just v)
        (toList d1)

-- Monoid instance
instance (Eq k, Hashable k) => Semigroup (SCDict k v) where
  (<>) = union

instance (Eq k, Hashable k) => Monoid (SCDict k v) where
  mempty = empty

-- Show instance 
instance (Show k, Show v) => Show (SCDict k v) where
  show dict = "SCDict " ++ show (toList dict)
