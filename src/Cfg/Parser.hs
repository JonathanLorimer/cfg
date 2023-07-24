{-# LANGUAGE DefaultSignatures #-}

module Cfg.Parser where

import Control.Error (note)
import Data.Functor (($>), void)
import Data.Text (Text)
import Data.Tree (Tree (..))
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.Megaparsec (Parsec, anySingle, between, empty, parseMaybe, sepBy, some, takeRest, try, (<|>), sepBy1, option)
import Text.Megaparsec.Char (digitChar, space1, string, string', char)
import Text.Megaparsec.Char.Lexer qualified as L
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty, fromList)
import qualified Data.Text as T
import Data.Int
import Data.Word

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
instance ValueParser a => ValueParser [a] where
  parser = between (L.symbol sp "[") (L.symbol sp "]") $ parser @a `sepBy` (L.symbol sp ",")

-- | @since 0.0.1.0
instance ValueParser a => ConfigParser [a]

-- | @since 0.0.1.0
instance ValueParser a => ValueParser (NonEmpty a) where
  parser = between (L.symbol sp "[") (L.symbol sp "]") $ fromList <$> parser @a `sepBy1` (L.symbol sp ",")

-- | @since 0.0.1.0
instance ValueParser a => ConfigParser (NonEmpty a)

-- | @since 0.0.1.0
instance ValueParser a => ValueParser (Maybe a) where
  parser =
    (try (string "Nothing") $> Nothing)
      <|> (L.symbol sp "Just" >> (Just <$> parser @a))

instance ValueParser a => ConfigParser (Maybe a)

-- Numeric Types

rd :: Read a => Text -> a
rd     = read . T.unpack

plus :: Parser Text
plus   = char '+' >> number

minus :: Parser Text
minus  = liftA2 (T.cons) (char '-') number

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
instance (ValueParser a, ValueParser b) => ValueParser (a,b) where
  parser = between (L.symbol sp "(") (L.symbol sp ")") $ do
    a <- parser @a 
    void $ L.symbol sp ","
    b <- parser @b
    pure (a,b)

-- | @since 0.0.1.0
instance (ValueParser a, ValueParser b) => ConfigParser (a,b)
