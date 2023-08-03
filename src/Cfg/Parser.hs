{-# LANGUAGE DefaultSignatures #-}

-- |
--  Module      : Cfg.Parser
--  Copyright   : © Jonathan Lorimer, 2023
--  License     : MIT
--  Maintainer  : jonathanlorimer@pm.me
--  Stability   : stable
--
-- @since 0.0.2.0
--
-- This module contains the type classes for parsing a configuration type from
-- a source, as well as instances for most basic Haskell types. One important
-- interaction to note is that we use a default instance for 'ConfigParser'
-- that dispatches to a 'ValueParser' instances. This is how we distinguish
-- between a \"parser\" that just navigates the tree representation of our
-- configuration and a parser that actually converts from text to our Haskell
-- type.
module Cfg.Parser
  ( -- * Parser Typeclasses
    ConfigParser (..)
  , ValueParser (..)

    -- * Parser Types
  , Parser
  , ConfigParseError (..)
  )
where

import Control.Error (note)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Functor (void)
import Data.Int
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Void (Void)
import Data.Word
import GHC.Generics (Generic)
import KeyTree
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

-- | Type alias for our megaparsec parser
--
-- @since 0.0.1.0
type Parser = Parsec Void Text

-- | Type errors we can encounter when parsing
--
-- @since 0.0.2.0
data ConfigParseError
  = -- | We encountered a 'Data.Map.Map' that was missing a key.
    MissingKey
      Text
      -- ^ The record field name that was missing.
      (KeyTree Text Text)
      -- ^ The subtree that was missing an entry.
  | -- | Expected to find a subtree aka a 'Free' with a map in it, but instead
    -- we found a 'Pure'.
    ExpectedKeyFoundValue
      Text
      -- ^ The key that was missing
      Text
      -- ^ The value that was found
  | -- | Expected to find a 'Pure' with a value but instead found a subtree
    ExpectedValueFoundForest
      (KeyTree Text Text)
      -- ^ The subtree that was found instead
  | -- | Ran a 'parser' and was unable to parse value
    ValueParseError
      Text
      -- ^ The parser error
  deriving (Eq, Show, Generic)

-- | This is the instance that allows us to parse a result from our
-- configuration source after we have retrieved it.
--
-- @since 0.0.2.0
class ConfigParser a where
  -- | Takes in the tree representation of our configuration and parses out our Haskell type
  --
  --  The default instance allows us to wrap a 'ValueParser' in a
  --  'ConfigParser', this allows us to use a uniform typeclass for parsing,
  --  but at the same time distinguish between traversing the key structure and
  --  actually parsing the textual value.
  parseConfig :: KeyTree Text Text -> Either ConfigParseError a
  default parseConfig :: (ValueParser a) => KeyTree Text Text -> Either ConfigParseError a
  parseConfig (Pure val) = note (ValueParseError val) $ parseMaybe parser val
  parseConfig kt = Left $ ExpectedValueFoundForest kt

-- | This is a text parser that we use to parse the eventual values we get from a
-- configuration.
--
-- @since 0.0.2.0
class ValueParser a where
  parser :: Parser a

-- | Lexer parser helper.
--
-- @since 0.0.2.0
sp :: Parsec Void Text ()
sp = L.space space1 empty empty

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

-- | @since 0.0.1.0
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
instance (ValueParser a) => ConfigParser (Maybe a) where
  parseConfig (Free m) =
    if m == M.empty
      then Right Nothing
      else Left $ ExpectedValueFoundForest (Free m)
  parseConfig (Pure v) = Just <$> note (ValueParseError v) (parseMaybe (parser @a) v)

-- Numeric parser helpers

-- | @since 0.0.2.0
rd :: (Read a) => Text -> a
rd = read . T.unpack

-- | @since 0.0.2.0
plus :: Parser Text
plus = char '+' >> number

-- | @since 0.0.2.0
minus :: Parser Text
minus = liftA2 (T.cons) (char '-') number

-- | @since 0.0.2.0
number :: Parser Text
number = T.pack <$> some digitChar

-- | @since 0.0.2.0
decimal :: Parser Text
decimal = option "" $ (T.cons) <$> char '.' <*> number

-- | @since 0.0.2.0
integral :: (Read a) => Parser a
integral = rd <$> (try plus <|> try minus <|> number)

-- | @since 0.0.2.0
fractional :: (Read a) => Parser a
fractional = fmap rd $ liftA2 (<>) integral decimal

-- | @since 0.0.1.0
instance ValueParser Double where
  parser = fractional

-- | @since 0.0.1.0
instance ConfigParser Double

-- | @since 0.0.1.0
instance ValueParser Float where
  parser = fractional

-- | @since 0.0.1.0
instance ConfigParser Float

-- | @since 0.0.1.0
instance ValueParser Int where
  parser = integral

-- | @since 0.0.1.0
instance ConfigParser Int

-- | @since 0.0.1.0
instance ValueParser Int8 where
  parser = integral

-- | @since 0.0.1.0
instance ConfigParser Int8

-- | @since 0.0.1.0
instance ValueParser Int16 where
  parser = integral

-- | @since 0.0.1.0
instance ConfigParser Int16

-- | @since 0.0.1.0
instance ValueParser Int32 where
  parser = integral

-- | @since 0.0.1.0
instance ConfigParser Int32

-- | @since 0.0.1.0
instance ValueParser Int64 where
  parser = integral

-- | @since 0.0.1.0
instance ConfigParser Int64

-- | @since 0.0.1.0
instance ValueParser Integer where
  parser = integral

-- | @since 0.0.1.0
instance ConfigParser Integer

-- | @since 0.0.1.0
instance ValueParser Word where
  parser = rd <$> number

-- | @since 0.0.1.0
instance ConfigParser Word

-- | @since 0.0.1.0
instance ValueParser Word8 where
  parser = rd <$> number

-- | @since 0.0.1.0
instance ConfigParser Word8

-- | @since 0.0.1.0
instance ValueParser Word16 where
  parser = rd <$> number

-- | @since 0.0.1.0
instance ConfigParser Word16

-- | @since 0.0.1.0
instance ValueParser Word32 where
  parser = rd <$> number

-- | @since 0.0.1.0
instance ConfigParser Word32

-- | @since 0.0.1.0
instance ValueParser Word64 where
  parser = rd <$> number

-- | @since 0.0.1.0
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
