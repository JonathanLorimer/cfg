module Cfg.Parser.ValueParser where

import Cfg.Parser
import GHC.Generics
import Data.Kind (Type)
import Text.Megaparsec
import Text.Megaparsec.Char (string)
import qualified Data.Text as T

defaultValueParser ::
    forall a.
    (Generic a, GValueParser (Rep a)) =>
    Parser a
defaultValueParser = fmap to $ gParser @(Rep a)

class GValueParser (f :: Type -> Type) where
    gParser :: Parser (f p)

instance GValueParser V1 where
    gParser = undefined

instance GValueParser U1 where
    gParser = string "()" >> pure U1

instance ValueParser a => GValueParser (K1 R a) where
    gParser = K1 <$> parser @a

instance GValueParser f => GValueParser (M1 D s f) where
    gParser = M1 <$> gParser @f

instance (Constructor c) => GValueParser (M1 C c U1) where
    gParser = M1 U1 <$ string (T.pack $ conName @c undefined)  

instance (GValueParser f) => GValueParser (M1 S s f) where
    gParser = M1 <$> gParser @f

instance (GValueParser a, GValueParser b) => GValueParser (a :*: b) where
    gParser = liftA2 (:*:) (gParser @a) (gParser @b)

instance (GValueParser a, GValueParser b) => GValueParser (a :+: b) where
    gParser = L1 <$> (try $ gParser @a) <|> R1 <$> (gParser @b)
