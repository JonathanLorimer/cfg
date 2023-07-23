module Cfg.Options where
import Data.Text (Text)

data RootOptions =
  RootOptions  
    { rootOptionsLabelModifier :: Text -> Text 
    , rootOptionsFieldOptions :: ConfigOptions
    }

defaultRootOptions :: RootOptions
defaultRootOptions = RootOptions id (ConfigOptions id)

data ConfigOptions =
  ConfigOptions  
    { configOptionsLabelModifier :: Text -> Text }

defaultConfigOptions :: ConfigOptions
defaultConfigOptions = ConfigOptions id

