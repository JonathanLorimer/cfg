-- |
--  Module      : Cfg.Deriving
--  Copyright   : © Jonathan Lorimer, 2023
--  License     : MIT
--  Maintainer  : jonathanlorimer@pm.me
--  Stability   : stable
--
-- @since 0.0.2.0
--
-- Convenience module that re-exports all the necessary bits for deriving
-- instances.
module Cfg.Deriving
  ( module Cfg.Deriving.Config
  , module Cfg.Deriving.Value
  , module Cfg.Deriving.KeyModifier
  )
where

import Cfg.Deriving.Config
import Cfg.Deriving.KeyModifier
import Cfg.Deriving.Value
