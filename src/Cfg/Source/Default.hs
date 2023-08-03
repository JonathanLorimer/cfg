-- |
--  Module      : Cfg.Source.Default
--  Copyright   : © Jonathan Lorimer, 2023
--  License     : MIT
--  Maintainer  : jonathanlorimer@pm.me
--  Stability   : stable
--
-- @since 0.0.2.0
--
-- This module contains code related to defaulting.
module Cfg.Source.Default where

import Data.Text (Text)


-- TODO: Figure out a way to make this error when there is a nested record

-- | This class lets us represent defaults as a function from record field
-- names to 'Maybe' values. @Nothing@ represents the absence of a default, and
-- the default implementation is to always return @Nothing@.
--
-- @since 0.0.2.0
class DefaultSource a where
  defaults 
    :: Text -- ^ Record field label
    -> Maybe Text -- ^ Serialized default
  defaults = const Nothing
