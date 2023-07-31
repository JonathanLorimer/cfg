module KeyTree (
  foldKeyTree,
  appendFold,
  mayAppendFold,
  appendTraverse,
  mayAppendTraverse,
  Map,
  Free(..),
  KeyTree,
  KeyForest,
) where

import Control.Monad.Free
import Data.Functor ((<&>))
import Data.Map.Strict

-- |
-- Pure value | Free (Map key (KeyTree key value))
-- @since 0.0.2.0
type KeyTree key value = Free (Map key) value

type KeyForest key value = Map key (Free (Map key) value)

foldKeyTree
  :: (Eq k, Eq v)
  => (v -> a)
  -> (k -> Free (Map k) v -> a -> a)
  -> a
  -> KeyTree k v
  -> a
foldKeyTree _ stepF acc (Free m) = foldrWithKey stepF acc m
foldKeyTree valF _ _ (Pure val) = valF val

appendFold
  :: (Eq k, Eq v)
  => (a -> v -> v')
  -> (a -> v')
  -> (a -> Map k (Free (Map k) v) -> a)
  -> a
  -> KeyTree k v
  -> KeyTree k v'
appendFold valF accF stepF acc (Free m) =
  if m == empty
    then Pure $ accF acc
    else Free $ appendFold valF accF stepF (stepF acc m) <$> m
appendFold valF _ _ acc (Pure v) = Pure $ valF acc v

mayAppendFold
  :: (Eq k, Eq v)
  => (a -> v -> v')
  -> (a -> Maybe v')
  -> (a -> Map k (Free (Map k) v) -> a)
  -> a
  -> KeyTree k v
  -> KeyTree k v'
mayAppendFold valF accF stepF acc (Free m) =
  if m == empty
    then case accF acc of
      Nothing -> Free empty
      Just v -> Pure v
    else Free $ mayAppendFold valF accF stepF (stepF acc m) <$> m
mayAppendFold valF _ _ acc (Pure v) = Pure $ valF acc v

appendTraverse
  :: (Applicative f, Eq k, Eq v)
  => (a -> v -> f v')
  -> (a -> f v')
  -> (a -> Map k (Free (Map k) v) -> a)
  -> a
  -> KeyTree k v
  -> f (KeyTree k v')
appendTraverse valF accF stepF acc = sequenceA . appendFold valF accF stepF acc

mayAppendTraverse
  :: (Applicative f, Eq k, Eq v)
  => (a -> v -> f v')
  -> (a -> f (Maybe v'))
  -> (k -> a -> Map k (Free (Map k) v) -> a)
  -> a
  -> KeyTree k v
  -> f (KeyTree k v')
mayAppendTraverse valF accF stepF acc (Free m) =
  if m == empty
    then
      accF acc <&> \case
        Nothing -> Free empty
        Just v -> Pure v
    else Free <$> traverseWithKey (\k v -> mayAppendTraverse valF accF stepF (stepF k acc m) v) m
mayAppendTraverse valF _ _ acc (Pure v) = Pure <$> valF acc v
