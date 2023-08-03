-- |
--  Module      : KeyTree
--  Copyright   : © Jonathan Lorimer, 2023
--  License     : MIT
--  Maintainer  : jonathanlorimer@pm.me
--  Stability   : stable
--
-- @since 0.0.2.0
--
-- This module contains types for our internal tree representation of types and
-- configurations. It also contains some helper functions for working with
-- these trees. This should make it easier to implement different source
-- providers.
module KeyTree
  ( -- * Types
    KeyTree
  , KeyForest

    -- * Helper Functions
  , foldKeyTree
  , appendFold
  , mayAppendFold
  , appendTraverse
  , mayAppendTraverse

    -- * Type Re-exports
  , Map
  , Free (..)
  )
where

import Control.Monad.Free
import Data.Functor ((<&>))
import Data.Map.Strict

-- | Type alias for our internal tree structure. If this was written directly
-- as a sum type it would look like this:
--
-- @Pure value | Free (Map key (KeyTree key value))@
--
-- @since 0.0.2.0
type KeyTree key value = Free (Map key) value

-- | Type alias for a subtree
--
-- @since 0.0.2.0
type KeyForest key value = Map key (Free (Map key) value)

-- | Right fold on a 'KeyTree'. Uses 'Data.Map.foldrWithKey' under the hood.
--
-- @since 0.0.2.0
foldKeyTree
  :: (Eq k, Eq v)
  => (v -> a)
  -- ^ Function to run on 'Pure' values
  -> (k -> Free (Map k) v -> a -> a)
  -- ^ Step function for fold
  -> a
  -- ^ Initial accumulator
  -> KeyTree k v
  -- ^ KeyTree to fold
  -> a
foldKeyTree _ stepF acc (Free m) = foldrWithKey stepF acc m
foldKeyTree valF _ _ (Pure val) = valF val

-- | A fold that appends a value at the leaf of the 'KeyTree', identifies what to
-- insert at the leaf by running a function on an accumulated value.
--
-- @since 0.0.2.0
appendFold
  :: (Eq k, Eq v)
  => (a -> v -> v')
  -- ^ Function to run on existing 'Pure' leaves @a@ is the accumulator @v@ is the value in @Pure@.
  -> (a -> v')
  -- ^ Function from accumulator @a@ to a value @v@. This happens when an empty subtree is found.
  -> (k -> a -> Map k (Free (Map k) v) -> a)
  -- ^ Step function for fold with key @k@ as an argument.
  -> a
  -- ^ Accumulator.
  -> KeyTree k v
  -- ^ KeyTree to append values to.
  -> KeyTree k v'
appendFold valF accF stepF acc (Free m) =
  if m == empty
    then Pure $ accF acc
    else Free $ mapWithKey (\k a -> appendFold valF accF stepF (stepF k acc m) a) m
appendFold valF _ _ acc (Pure v) = Pure $ valF acc v

-- | A fold that appends a value at the leaf of the 'KeyTree' (like
-- 'appendFold'), but the function on the accumulator can return a 'Maybe'. In
-- the @Nothing@ case we just append a 'Free' full of an empty 'Data.Map.Map'.
--
-- @since 0.0.2.0
mayAppendFold
  :: (Eq k, Eq v)
  => (a -> v -> Maybe v')
  -- ^ Function to run on existing 'Pure' leaves.
  -> (a -> Maybe v')
  -- ^ Function to run when an empty node is found.
  -> (k -> a -> Map k (Free (Map k) v) -> a)
  -- ^ Step function for fold with key @k@ as an argument.
  -> a
  -- ^ Accumulator.
  -> KeyTree k v
  -- ^ Tree to be folded.
  -> KeyTree k v'
mayAppendFold valF accF stepF acc (Free m) =
  if m == empty
    then case accF acc of
      Nothing -> Free empty
      Just v -> Pure v
    else Free $ mapWithKey (\k a -> mayAppendFold valF accF stepF (stepF k acc m) a) m
mayAppendFold valF _ _ acc (Pure v) =
  case valF acc v of
    Nothing -> Free empty
    Just v' -> Pure v'

-- | Like 'appendFold' but with functions that return a value wrapped in an
-- 'Applicative' effect @f@. The function is suffixed with \"Traverse\" because
-- we use the 'sequenceA' to wrap the entire tree in a single 'Applicative'
-- effect.
--
-- @since 0.0.2.0
appendTraverse
  :: (Applicative f, Eq k, Eq v)
  => (a -> v -> f v')
  -- ^ Function to run on existing 'Pure' leaves.
  -> (a -> f v')
  -- ^ Function to run when an empty node is found.
  -> (k -> a -> Map k (Free (Map k) v) -> a)
  -- ^ Step function for fold with key @k@ as an argument.
  -> a
  -- ^ Accumulator.
  -> KeyTree k v
  -- ^ Tree to be folded.
  -> f (KeyTree k v')
appendTraverse valF accF stepF acc = sequenceA . appendFold valF accF stepF acc

-- | Similar to 'appendTraverse' except the 'Applicative' effect can optionally
-- return a result. The function is manually implemented rather than using
-- 'Traversable' methods so that we don't have to require 'Traversable' on @f@.
-- This allows consumers to use effects like 'IO' that don't have a
-- 'Traversable' instance.
--
-- @since 0.0.2.0
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
