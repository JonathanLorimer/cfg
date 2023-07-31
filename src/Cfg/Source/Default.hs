module Cfg.Source.Default where

import Data.Text (Text)

-- TODO: Figure out a way to make this error when there is a nested record
class DefaultSource a where
  defaults :: Text -> Maybe Text
  defaults = const Nothing


