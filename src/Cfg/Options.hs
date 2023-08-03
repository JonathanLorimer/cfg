-- |
--  Module      : Cfg.Options
--  Copyright   : © Jonathan Lorimer, 2023
--  License     : MIT
--  Maintainer  : jonathanlorimer@pm.me
--  Stability   : stable
--
-- @since 0.0.2.0
--
-- This module contains the options types that we pass to our generic
-- functions. Mostly these are used for registering text transformations on
-- keys, but another interesting use case is choosing between the type
-- constructor and the data constructor for deriving a name for root keys.
module Cfg.Options where

import Data.Text (Text)

-- | Options that pertain to record field accessors.
--
-- @since 0.0.1.0
data KeyOptions = KeyOptions
  { keyOptionsModifier :: Text -> Text
  }

-- | Default key options, does no transformation to record field accessors.
--
-- @since 0.0.2.0
defaultKeyOptions :: KeyOptions
defaultKeyOptions = KeyOptions id

-- | Type that represents a decision between using the type constructor name or
-- the data constructor name as the root key.
--
-- This type is polymorphic so that we can use it to contain a term level text
-- transformation for root keys, as well as be used at the type level
-- parameterized by a type that defines the key modifiers to use.
--
-- @since 0.0.2.0
data RootKey a = ConstructorName a | TypeName a

-- | Options for manipulating a root key
--
-- @since 0.0.2.0
data RootOptions = RootOptions
  { rootOptionsRootKey :: RootKey (Text -> Text)
  , rootOptionsModifier :: Text -> Text
  }

-- | Default root key option, uses the type constructor name for the root key
-- and applies no transformations to the root key or keys derived from record
-- fields.
--
-- @since 0.0.2.0
defaultRootOptions :: RootOptions
defaultRootOptions = RootOptions (TypeName id) id

-- | Represents all possible kinds of configuration options.
--
-- @since 0.0.2.0
data ConfigOptions = Root RootOptions | Key KeyOptions

-- | Defaults to regular 'KeyOptions' (not 'RootOptions')
--
-- @since 0.0.2.0
defaultConfigOptions :: ConfigOptions
defaultConfigOptions = Key defaultKeyOptions

-- | Helper function that allows us to generically extract the record field
-- modifiers from either a 'RootOptions' or a 'KeyOptions' record.
--
-- @since 0.0.2.0
keyModifier :: ConfigOptions -> (Text -> Text)
keyModifier (Root options) = rootOptionsModifier options
keyModifier (Key options) = keyOptionsModifier options
