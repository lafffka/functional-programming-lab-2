module Dict
  ( Dict (..),
  )
where

import Data.Hashable (Hashable)
import Prelude hiding (lookup)

class Dict d where
  empty :: d k v

  insert ::
    (Hashable k) =>
    k ->
    v ->
    d k v ->
    d k v

  delete ::
    (Hashable k) =>
    k ->
    d k v ->
    d k v

  lookup ::
    (Hashable k) =>
    k ->
    d k v ->
    Maybe v

  mapValues ::
    (v -> w) ->
    d k v ->
    d k w

  filterDict ::
    (k -> v -> Bool) ->
    d k v ->
    d k v

  foldLeft ::
    (acc -> (k, v) -> acc) ->
    acc ->
    d k v ->
    acc

  foldRight ::
    ((k, v) -> acc -> acc) ->
    acc ->
    d k v ->
    acc

  size :: d k v -> Int
  keys :: d k v -> [k]
  values :: d k v -> [v]
  toList :: d k v -> [(k, v)]
  fromList ::
    (Hashable k) =>
    [(k, v)] ->
    d k v

  union ::
    (Hashable k) =>
    d k v ->
    d k v ->
    d k v
