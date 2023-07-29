module KeyTree where

import Control.Monad.Free
import Data.Functor ((<&>))
import Data.Map

-- |
-- Pure value | Free (Map key (KeyTree key value))
-- @since 0.0.2.0
type KeyTree key value = Free (Map key) value

appendFold'
  :: (Eq k, Eq v)
  => (v -> v')
  -> (a -> v')
  -> (a -> Map k (Free (Map k) v) -> a)
  -> a
  -> KeyTree k v
  -> KeyTree k v'
appendFold' valF accF stepF acc (Free m) =
  if m == empty
    then Pure $ accF acc
    else Free $ appendFold' valF accF stepF (stepF acc m) <$> m
appendFold' valF _ _ _ (Pure v) = Pure $ valF v

appendFold
  :: (Eq k, Eq v) => (a -> v) -> (a -> Map k (Free (Map k) v) -> a) -> a -> KeyTree k v -> KeyTree k v
appendFold = appendFold' id

mayAppendFold'
  :: (Eq k, Eq v)
  => (v -> v')
  -> (a -> Maybe v')
  -> (a -> Map k (Free (Map k) v) -> a)
  -> a
  -> KeyTree k v
  -> KeyTree k v'
mayAppendFold' valF accF stepF acc (Free m) =
  if m == empty
    then case accF acc of
      Nothing -> Free empty
      Just v -> Pure v
    else Free $ mayAppendFold' valF accF stepF (stepF acc m) <$> m
mayAppendFold' valF _ _ _ (Pure v) = Pure $ valF v

mayAppendFold
  :: (Eq k, Eq v)
  => (a -> Maybe v)
  -> (a -> Map k (Free (Map k) v) -> a)
  -> a
  -> KeyTree k v
  -> KeyTree k v
mayAppendFold = mayAppendFold' id

appendTraverse'
  :: (Applicative f, Eq k, Eq v)
  => (v -> f v')
  -> (a -> f v')
  -> (a -> Map k (Free (Map k) v) -> a)
  -> a
  -> KeyTree k v
  -> f (KeyTree k v')
appendTraverse' valF accF stepF acc = sequenceA . appendFold' valF accF stepF acc

appendTraverse
  :: (Applicative f, Eq k, Eq v)
  => (a -> f v)
  -> (a -> Map k (Free (Map k) v) -> a)
  -> a
  -> KeyTree k v
  -> f (KeyTree k v)
appendTraverse = appendTraverse' pure

mayAppendTraverse'
  :: (Applicative f, Eq k, Eq v)
  => (v -> f v')
  -> (a -> f (Maybe v'))
  -> (a -> Map k (Free (Map k) v) -> a)
  -> a
  -> KeyTree k v
  -> f (KeyTree k v')
mayAppendTraverse' valF accF stepF acc (Free m) =
  if m == empty
    then
      accF acc <&> \case
        Nothing -> Free empty
        Just v -> Pure v
    else Free <$> traverse (mayAppendTraverse' valF accF stepF (stepF acc m)) m
mayAppendTraverse' valF _ _ _ (Pure v) = Pure <$> valF v

mayAppendTraverse
  :: (Applicative f, Eq k, Eq v)
  => (a -> f (Maybe v))
  -> (a -> Map k (Free (Map k) v) -> a)
  -> a
  -> KeyTree k v
  -> f (KeyTree k v)
mayAppendTraverse = mayAppendTraverse' pure
