-- |
--  Module      : Cfg
--  Copyright   : © Jonathan Lorimer, 2023
--  License     : MIT
--  Maintainer  : jonathanlorimer@pm.me
--  Stability   : stable
--
-- @since 0.0.2.0
--
-- This module provides type level assertions so that we can constrain the
-- instances user's can create, and give them good error messages
module Cfg.Deriving.Assert where

import Data.Kind (Type)
import GHC.Base (Constraint)
import GHC.Generics
import GHC.TypeError (ErrorMessage (..), TypeError)

-- | A type level helper for creating custom error messages based on a predicate
--
-- @since 0.0.2.0
class Assert (pred :: Bool) (msg :: ErrorMessage)

instance Assert 'True msg

instance (TypeError msg ~ '()) => Assert 'False msg

-- | A type level predicate that helps us identify top level product types
--
-- @since 0.0.2.0
type family IsTopLevelRecord f where
  IsTopLevelRecord V1 = 'False
  IsTopLevelRecord U1 = 'False
  IsTopLevelRecord (K1 i c) = 'False
  IsTopLevelRecord (M1 i c f) = IsTopLevelRecord f
  IsTopLevelRecord (f :*: g) = 'True
  IsTopLevelRecord (f :+: g) = 'False

-- | A custom error message for non-top-level records
--
-- @since 0.0.2.0
type AssertTopLevelRecord (constraint :: Type -> Constraint) a =
  Assert
    (IsTopLevelRecord (Rep a))
    ( 'Text "🚫 Cannot derive "
        ':<>: 'ShowType constraint
        ':<>: 'Text " instance for "
        ':<>: 'ShowType a
        ':$$: ( 'Text "💡 "
                  ':<>: 'ShowType constraint
                  ':<>: 'Text " must be derived on a top level record type with named fields."
              )
    )
