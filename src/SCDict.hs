module SCDict
    ( Dict
    , empty
    , insert
    , delete
    , lookup
    , mapValues
    , filterDict
    , foldLeft
    , foldRight
    , size
    , keys
    , values
    , toList
    , union
    ) where

import Prelude hiding (lookup)
import qualified Data.List as List
import Data.Hashable (Hashable, hash)

type Bucket k v = [(k, v)]

data Dict k v = Dict
    { numBuckets :: Int
    , bucketList :: [Bucket k v]
    } deriving (Show)

instance (Hashable k, Eq k, Eq v) => Eq (Dict k v) where
    d1 == d2 = size d1 == size d2 && all (\(k, v) -> lookup k d2 == Just v) (toList d1)

-- empty
empty :: Dict k v
empty = Dict { numBuckets = defaultSize, bucketList = replicate defaultSize [] }
    where defaultSize = 16

bucketIndex :: Hashable k => k -> Int -> Int
bucketIndex key bucketsCount = abs (hash key) `mod` bucketsCount

updateAt :: Int -> a -> [a] -> [a]
updateAt _ _ [] = []
updateAt 0 new (_:rest) = new : rest
updateAt i new (x:rest) = x : updateAt (i-1) new rest

insertInBucket :: Eq k => k -> v -> Bucket k v -> Bucket k v
insertInBucket key val [] = [(key, val)]
insertInBucket key val ((k, v) : rest)
    | key == k = (key, val) : rest
    | otherwise = (k, v) : insertInBucket key val rest

-- insert operation
insert :: (Hashable k) => k -> v -> Dict k v -> Dict k v
insert key val hmap = 
    let idx = bucketIndex key ( numBuckets hmap )
        oldBucket = bucketList hmap !! idx
        newBucket = insertInBucket key val oldBucket
        newBuckets = updateAt idx newBucket (bucketList hmap)
    in hmap { bucketList = newBuckets }

deleteFromBucket :: Eq k => k -> Bucket k v -> Bucket k v
deleteFromBucket _ [] = []
deleteFromBucket key ((k, v) : rest)
    | key == k = rest
    | otherwise = (k, v) : deleteFromBucket key rest

-- delete operation
delete :: (Hashable k) => k -> Dict k v -> Dict k v
delete key hmap =
    let idx = bucketIndex key ( numBuckets hmap )
        oldBucket = bucketList hmap !! idx
        newBucket = deleteFromBucket key oldBucket
        newBuckets = updateAt idx newBucket ( bucketList hmap )
    in hmap { bucketList = newBuckets }

-- lookup operation
lookup :: (Hashable k) => k -> Dict k v -> Maybe v
lookup key hmap = 
    let idx = bucketIndex key ( numBuckets hmap )
        bucket = bucketList hmap !! idx
    in List.lookup key bucket


foldLeftBucket :: (acc -> (k, v) -> acc) -> acc -> Bucket k v -> acc
foldLeftBucket _ acc [] = acc
foldLeftBucket f acc (pair:rest) = foldLeftBucket f (f acc pair) rest

foldRightBucket :: ((k, v) -> acc -> acc) -> acc -> Bucket k v -> acc
foldRightBucket _ acc [] = acc
foldRightBucket f acc (pair:rest) = f pair (foldRightBucket f acc rest)

-- foldLeft operation
foldLeft :: (acc -> (k, v) -> acc) -> acc -> Dict k v -> acc
-- foldLeft f initAcc hmap = foldl' (foldl' f) initAcc (bucketList hmap)
foldLeft f initAcc hmap = go initAcc (bucketList hmap)
    where
        go acc [] = acc
        go acc (bucket:rest) = go (foldLeftBucket f acc bucket) rest

-- foldRight operation
foldRight :: ((k, v) -> acc -> acc) -> acc -> Dict k v -> acc
-- foldRight f initAcc hmap = foldr (\bucket acc -> foldr f acc bucket) initAcc (bucketList hmap)
foldRight f initAcc hmap = go (bucketList hmap)
    where
        go [] = initAcc
        go (bucket:rest) = foldRightBucket f (go rest) bucket

size :: Dict k v -> Int
-- size hmap = foldLeft (\acc _ -> acc + 1) 0 hmap
size hmap = foldRight (\_ acc -> acc + 1) 0 hmap

keys :: Dict k v -> [k]
-- keys hmap = foldRight (\(k, _) acc -> k : acc) [] hmap
keys hmap = foldLeft (\acc (k, _) -> k : acc) [] hmap

values :: Dict k v -> [v]
-- values hmap = foldRight (\(_, v) acc -> v : acc) [] hmap
values hmap = foldLeft (\acc (_, v) -> v : acc) [] hmap

toList :: Dict k v -> [(k, v)]
toList hmap = foldRight (:) [] hmap
-- toList hmap = foldLeft (\acc pair -> pair : acc) [] hmap

mapPair :: (v -> w) -> (k, v) -> (k, w)
mapPair f (k, v) = (k, f v)

-- mapBucket
mapBucket :: (v -> w) -> Bucket k v -> Bucket k w
mapBucket f bucket = map (mapPair f) bucket

mapValues :: (v -> w) -> Dict k v -> Dict k w
mapValues f hmap = hmap { bucketList = map (mapBucket f) (bucketList hmap) }

filterBucket :: (k -> v -> Bool) -> Bucket k v -> Bucket k v
-- filterBucket p bucket = filter (\(k, v) -> p k v)
filterBucket _ [] = []
filterBucket p ((k, v) : rest)
    | p k v = (k, v) : filterBucket p rest
    | otherwise = filterBucket p rest

-- filterDict
filterDict :: (k -> v -> Bool) -> Dict k v -> Dict k v
filterDict p hmap = hmap { bucketList = map (filterBucket p) (bucketList hmap) }

-- union
union :: (Hashable k) => Dict k v -> Dict k v -> Dict k v
union d1 d2 = foldLeft (\acc (k, v) -> insert k v acc) d2 d1

instance (Hashable k, Eq k) => Semigroup (Dict k v) where
    (<>) = union

instance (Hashable k, Eq k) => Monoid (Dict k v) where
    mempty = empty