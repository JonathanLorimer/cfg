module Cfg.Options where

import Data.Text (Text)

data KeyOptions = KeyOptions
  { keyOptionsModifier :: Text -> Text
  }

defaultKeyOptions :: KeyOptions
defaultKeyOptions = KeyOptions id

data RootKey = DataCon (Text -> Text) | TyCon (Text -> Text)

data RootOptions = RootOptions
  { rootOptionsRootKey :: RootKey
  , rootOptionsModifier :: Text -> Text
  }

defaultRootOptions :: RootOptions
defaultRootOptions = RootOptions (TyCon id) id 

data ConfigOptions = Root RootOptions | Key KeyOptions

defaultConfigOptions :: ConfigOptions
defaultConfigOptions = Key defaultKeyOptions 

keyModifier :: ConfigOptions -> (Text -> Text)
keyModifier (Root options) = rootOptionsModifier options
keyModifier (Key options) = keyOptionsModifier options
