module Cfg.Parser where

import Text.Parsec (Parsec)
import Data.Void (Void)
import Data.Text (Text)
import Data.Tree (Tree)

type Parser = Parsec Void Text

class CfgParser a where
  parse :: Tree Text -> Parser a
