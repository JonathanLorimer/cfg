module Cfg.Deriving.LabelModifier where

import Data.Text (Text)
import Data.Text qualified as T

data ToLower

data ToUpper

data Ident

class LabelModifier t where
    getLabelModifier :: Text -> Text

instance LabelModifier ToLower where
    getLabelModifier = T.toLower

instance LabelModifier ToUpper where
    getLabelModifier = T.toUpper

instance LabelModifier Ident where
    getLabelModifier = id
