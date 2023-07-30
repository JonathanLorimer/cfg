{-# LANGUAGE DefaultSignatures #-}

module Cfg.Parser where

import Control.Error (note)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Functor (void) 
import Data.Int
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Void (Void)
import Data.Word
import GHC.Generics (Generic)
import Text.Megaparsec
  ( Parsec
  , anySingle
  , between
  , empty
  , option
  , parseMaybe
  , sepBy
  , sepBy1
  , some
  , takeRest
  , try
  , (<|>)
  )
import Text.Megaparsec.Char (char, digitChar, space1, string, string')
import Text.Megaparsec.Char.Lexer qualified as L
import KeyTree
import qualified Data.Map.Strict as M

type Parser = Parsec Void Text

data ConfigParseError
  = MismatchedKeyAndField Text (Text, Text)
  | MissingKeys [Text] (KeyTree Text Text)
  | ExpectedKeyFoundValue Text Text
  | MissingValue Text
  | ExpectedForestFoundValue Text
  | ExpectedValueFoundForest (KeyTree Text Text)
  | UnexpectedKeys (KeyTree Text Text)
  | ValueParseError Text
  deriving (Eq, Show, Generic)

class ConfigParser a where
  parseConfig :: KeyTree Text Text -> Either ConfigParseError a
  default parseConfig :: (ValueParser a) => KeyTree Text Text -> Either ConfigParseError a
  parseConfig (Pure val) = note (ValueParseError val) $ parseMaybe parser val
  parseConfig kt = Left $ UnexpectedKeys kt

-- class NestedParser a where
--   parseNestedConfig :: KeyTree Text Text -> Either ConfigParseError a
--   default parseNestedConfig :: (ValueParser a) => KeyTree Text Text -> Either ConfigParseError a
--   parseNestedConfig (Pure val) = note (ValueParseError val) $ parseMaybe parser val
--   parseNestedConfig kt = Left $ UnexpectedKeys kt

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

-- | @since 0.0.1.0
instance ValueParser Char where
  parser = anySingle

-- | @since 0.0.1.0
instance ConfigParser Char

-- | @since 0.0.1.0
instance ValueParser TL.Text where
  parser = TL.fromStrict <$> takeRest

-- | @since 0.0.1.0
instance ConfigParser TL.Text

-- | @since 0.0.1.0
instance ValueParser BL.ByteString where
  parser = BL.fromStrict . encodeUtf8 <$> takeRest

-- | @since 0.0.1.0
instance ConfigParser BL.ByteString

-- | @since 0.0.1.0
instance ValueParser BS.ByteString where
  parser = encodeUtf8 <$> takeRest

-- | @since 0.0.1.0
instance ConfigParser BS.ByteString

-- @since 0.0.1.0
instance ValueParser Text where
  parser = takeRest

-- | @since 0.0.1.0
instance ConfigParser Text

-- | @since 0.0.1.0
instance (ValueParser a) => ValueParser [a] where
  parser = between (L.symbol sp "[") (L.symbol sp "]") $ parser @a `sepBy` (L.symbol sp ",")

-- | @since 0.0.1.0
instance (ValueParser a) => ConfigParser [a]

-- | @since 0.0.1.0
instance (ValueParser a) => ValueParser (NonEmpty a) where
  parser = between (L.symbol sp "[") (L.symbol sp "]") $ fromList <$> parser @a `sepBy1` (L.symbol sp ",")

-- | @since 0.0.1.0
instance (ValueParser a) => ConfigParser (NonEmpty a)

-- | @since 0.0.1.0
-- TODO: Need to implement this better
-- instance (ValueParser a) => ValueParser (Maybe a) where
--   parser =
--     (try (string "Nothing") $> Nothing)
--       <|> (L.symbol sp "Just" >> (Just <$> parser @a))

instance (ValueParser a) => ConfigParser (Maybe a) where
  parseConfig (Free m) = 
    if m == M.empty 
       then Right Nothing 
       else Left $ ExpectedValueFoundForest (Free m)
  parseConfig (Pure v) = Just <$> note (ValueParseError v) (parseMaybe (parser @a) v)

-- Numeric Types

rd :: (Read a) => Text -> a
rd = read . T.unpack

plus :: Parser Text
plus = char '+' >> number

minus :: Parser Text
minus = liftA2 (T.cons) (char '-') number

number :: Parser Text
number = T.pack <$> some digitChar

decimal :: Parser Text
decimal = option "" $ (T.cons) <$> char '.' <*> number

integral :: (Read a) => Parser a
integral = rd <$> (try plus <|> try minus <|> number)

fractional :: (Read a) => Parser a
fractional = fmap rd $ liftA2 (<>) integral decimal

-- | @since 0.0.1.0
instance ValueParser Double where
  parser = fractional

instance ConfigParser Double

-- | @since 0.0.1.0
instance ValueParser Float where
  parser = fractional

instance ConfigParser Float

-- @since 0.0.1.0
instance ValueParser Int where
  parser = integral

instance ConfigParser Int

-- | @since 0.0.1.0
instance ValueParser Int8 where
  parser = integral

instance ConfigParser Int8

-- | @since 0.0.1.0
instance ValueParser Int16 where
  parser = integral

instance ConfigParser Int16

-- | @since 0.0.1.0
instance ValueParser Int32 where
  parser = integral

instance ConfigParser Int32

-- | @since 0.0.1.0
instance ValueParser Int64 where
  parser = integral

instance ConfigParser Int64

-- | @since 0.0.1.0
instance ValueParser Integer where
  parser = integral

instance ConfigParser Integer

-- | @since 0.0.1.0
instance ValueParser Word where
  parser = rd <$> number

instance ConfigParser Word

-- | @since 0.0.1.0
instance ValueParser Word8 where
  parser = rd <$> number

instance ConfigParser Word8

-- | @since 0.0.1.0
instance ValueParser Word16 where
  parser = rd <$> number

instance ConfigParser Word16

-- | @since 0.0.1.0
instance ValueParser Word32 where
  parser = rd <$> number

instance ConfigParser Word32

-- | @since 0.0.1.0
instance ValueParser Word64 where
  parser = rd <$> number

instance ConfigParser Word64

-- | @since 0.0.1.0
instance (ValueParser a, ValueParser b) => ValueParser (a, b) where
  parser = between (L.symbol sp "(") (L.symbol sp ")") $ do
    a <- parser @a
    void $ L.symbol sp ","
    b <- parser @b
    pure (a, b)

-- | @since 0.0.1.0
instance (ValueParser a, ValueParser b) => ConfigParser (a, b)
