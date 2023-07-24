{-# LANGUAGE DefaultSignatures #-}

module Cfg.Parser where

import Control.Error (note)
import Data.Text (Text)
import Data.Tree (Tree (..))
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.Megaparsec (Parsec, parseMaybe, some, try, (<|>), anySingle, takeRest, empty, between, sepBy)
import Text.Megaparsec.Char (digitChar, string, string', space1)
import Data.Functor (($>))
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data ConfigParseError
    = UnmatchedFields [Tree Text]
    | MismatchedRootKey Text Text
    | MismatchedKeyAndField Text (Text, Text)
    | MissingKeys [Text]
    | MissingValue Text
    | UnexpectedKeys Text [Tree Text]
    | ValueParseError Text
    deriving (Eq, Show, Generic)

class RootParser a where
    parseRootConfig :: Tree Text -> Either ConfigParseError a

class ConfigParser a where
    parseConfig :: Tree Text -> Either ConfigParseError a

    default parseConfig :: (ValueParser a) => Tree Text -> Either ConfigParseError a
    parseConfig (Node val []) = note (ValueParseError val) $ parseMaybe parser val
    parseConfig (Node label xs) = Left $ UnexpectedKeys label xs

sp :: Parsec Void Text ()
sp = L.space space1 empty empty

class ValueParser a where
    parser :: Parser a

-- | @since 0.0.1.0
instance ValueParser () where
    parser = string "()" >> pure ()

-- | @since 0.0.1.0
instance ConfigParser ()

-- | @since 0.0.1.0
instance ValueParser Bool where
    parser = try (string' "true" >> pure True) <|> (string' "false" >> pure False)

-- | @since 0.0.1.0
instance ConfigParser Bool

-- -- | @since 0.0.1.0
instance ValueParser Char where
    parser = anySingle

-- -- | @since 0.0.1.0
instance ConfigParser Char

-- -- @since 0.0.1.0
-- deriving via (ConfigValue TL.Text) instance NestedConfig TL.Text
--
-- -- @since 0.0.1.0
-- deriving via (ConfigValue BL.ByteString) instance NestedConfig BL.ByteString
--
-- -- @since 0.0.1.0
-- deriving via (ConfigValue BS.ByteString) instance NestedConfig BS.ByteString
--
-- -- @since 0.0.1.0
instance ValueParser Text where
    parser = takeRest

instance ConfigParser Text

-- @since 0.0.1.0
instance ValueParser a => ValueParser [a] where
    parser = between (L.symbol sp "[") (L.symbol sp "]") $ parser @a `sepBy` (L.symbol sp ",") 

instance ValueParser a => ConfigParser [a]
--
-- -- @since 0.0.1.0
-- deriving via (ConfigValue (NonEmpty a)) instance NestedConfig (NonEmpty a)
--
-- -- @since 0.0.1.0
-- deriving via (ConfigValue (Vector a)) instance NestedConfig (Vector a)
--
-- -- @since 0.0.1.0
instance ValueParser a => ValueParser (Maybe a) where
    parser = (try (string "Nothing") $> Nothing) 
      <|> (L.symbol sp "Just" >> (Just <$> parser @a))

instance ValueParser a => ConfigParser (Maybe a)
--
-- -- @since 0.0.1.0
-- deriving via (ConfigValue Double) instance NestedConfig Double
--
-- -- @since 0.0.1.0
-- deriving via (ConfigValue Float) instance NestedConfig Float
--
-- @since 0.0.1.0
instance ValueParser Int where
    parser = read <$> some digitChar

instance ConfigParser Int

-- -- @since 0.0.1.0
-- deriving via (ConfigValue Int8) instance NestedConfig Int8
--
-- -- @since 0.0.1.0
-- deriving via (ConfigValue Int16) instance NestedConfig Int16
--
-- -- @since 0.0.1.0
-- deriving via (ConfigValue Int32) instance NestedConfig Int32
--
-- -- @since 0.0.1.0
-- deriving via (ConfigValue Int64) instance NestedConfig Int64
--
-- -- @since 0.0.1.0
-- deriving via (ConfigValue Integer) instance NestedConfig Integer
--
-- -- @since 0.0.1.0
-- deriving via (ConfigValue Word) instance NestedConfig Word
--
-- -- @since 0.0.1.0
-- deriving via (ConfigValue Word8) instance NestedConfig Word8
--
-- -- @since 0.0.1.0
-- deriving via (ConfigValue Word16) instance NestedConfig Word16
--
-- -- @since 0.0.1.0
-- deriving via (ConfigValue Word32) instance NestedConfig Word32
--
-- -- @since 0.0.1.0
-- deriving via (ConfigValue Word64) instance NestedConfig Word64
--
-- -- @since 0.0.1.0
-- deriving via (ConfigValue (a,b)) instance NestedConfig (a,b)
