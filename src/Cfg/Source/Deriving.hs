{-# LANGUAGE UndecidableInstances #-}
module Cfg.Source.Deriving where

import GHC.Generics 
import GHC.TypeError (ErrorMessage (..), TypeError)
import Data.Kind (Type)
import GHC.Base (Constraint)
import Data.Data (Proxy(..))
import Cfg.Source.RootConfig
import Cfg.Source.NestedConfig
import Cfg.Source
import qualified Data.Text as T
import Data.Text (Text)

class Assert (pred :: Bool) (msg :: ErrorMessage)
instance Assert 'True msg
instance TypeError msg ~ '() => Assert 'False msg

-- -- | This wrapper allows you to create an `Display` instance for a record,
-- -- so long as all the record fields have a `Display` instance as well.
-- --
-- -- === Example
-- --
-- -- > data Password = Password
-- -- >  deriving Display
-- -- >    via (OpaqueInstance "[REDACTED]" Password)
-- --
-- -- > data MyRecord =
-- -- >    MyRecord
-- -- >      { fieldA :: String
-- -- >      , fieldB :: Maybe String
-- -- >      , fieldC :: Int
-- -- >      , pword :: Password
-- -- >      }
-- -- >      deriving stock (Generic)
-- -- >      deriving (Display) via (RecordInstance MyRecord)
-- --
-- -- > putStrLn . Data.Text.unpack . display $ MyRecord "hello" (Just "world") 22 Password
-- --
-- -- > MyRecord
-- -- >   { fieldA = hello
-- -- >   , fieldB = Just world
-- -- >   , fieldC = 22
-- -- >   , pword = [REDACTED]
-- -- >   }
-- --
-- -- @since 0.0.5.0
newtype ConfigRoot a = ConfigRoot {unConfigRoot :: a}

instance Generic a => Generic (ConfigRoot a) where
  type Rep (ConfigRoot a) = Rep a
  to = ConfigRoot . to
  from (ConfigRoot x) = from x
--
-- -- | We leverage the `AssertNoSum` type family to prevent consumers
-- -- from deriving instances for sum types. Sum types should use a manual instance
-- -- or derive one via `ShowInstance`.
-- --
-- -- @since 0.0.5.0
instance (AssertTopLevelRecord RootConfig a, Generic a, GConfigTree1 (Rep a)) => RootConfig (ConfigRoot a) where
  toRootConfig _ = defaultToRootConfig defaultRootOptions (Proxy @a)

newtype ConfigRootOpts t t' a = ConfigRootOpts {unConfigRootOpts :: a}

instance Generic a => Generic (ConfigRootOpts t t' a) where
  type Rep (ConfigRootOpts t t' a) = Rep a
  to = ConfigRootOpts . to
  from (ConfigRootOpts x) = from x

instance (LabelModifier t, LabelModifier t', AssertTopLevelRecord RootConfig a, Generic a, GConfigTree1 (Rep a)) => RootConfig (ConfigRootOpts t t' a) where
  toRootConfig _ = defaultToRootConfig (getConfigRootOptions @t @t') (Proxy @a)

class (LabelModifier t, LabelModifier t') => GetConfigRootOptions t t' where
  getConfigRootOptions :: RootOptions

instance (LabelModifier t, LabelModifier t') => GetConfigRootOptions t t' where
  getConfigRootOptions = RootOptions (getLabelModifier @t) (ConfigOptions $ getLabelModifier @t')

-- -- | This type family is lifted from generic-data. We use it to prevent the user from
-- -- deriving a `RecordInstance` for sum types
-- --
-- -- @since 0.0.5.0
type family IsTopLevelRecord f where
  IsTopLevelRecord V1 = 'False
  IsTopLevelRecord U1 = 'False
  IsTopLevelRecord (K1 i c) = 'False
  IsTopLevelRecord (M1 i c f) = IsTopLevelRecord f
  IsTopLevelRecord (f :*: g) = 'True
  IsTopLevelRecord (f :+: g) = 'False

-- | Constraint to prevent misuse of `RecordInstance` deriving via mechanism.
--
-- === Example
--
-- > data MySum = A | B | C deriving stock (Generic) deriving (Display) via (RecordInstance MySum)
--
-- >    • 🚫 Cannot derive Display instance for MySum via RecordInstance due to sum type
-- >      💡 Sum types should use a manual instance or derive one via ShowInstance.
-- >    • When deriving the instance for (Display MySum)
--
-- @since 0.0.5.0
type AssertTopLevelRecord (constraint :: Type -> Constraint) a =
  Assert
    (IsTopLevelRecord (Rep a))
    ( 'Text "🚫 Cannot derive "
        ':<>: 'ShowType constraint
        ':<>: 'Text " instance for "
        ':<>: 'ShowType a
        ':<>: 'Text " via ConfigRoot"
        ':$$: 'Text "💡 ConfigRoot must be derived on a top level record type with named fields."
    )

newtype SubConfig a = SubConfig {unSubConfig :: a}

instance Generic a => Generic (SubConfig a) where
  type Rep (SubConfig a) = Rep a
  to = SubConfig . to
  from (SubConfig x) = from x

instance (Generic a, GConfigForest1 (Rep a)) => NestedConfig (SubConfig a) where
  toNestedConfig _ = defaultToNestedConfig defaultConfigOptions (Proxy @a)

newtype SubConfigOpts t a = SubConfigOpts {unSubConfigOpts :: a}

instance Generic a => Generic (SubConfigOpts t a) where
  type Rep (SubConfigOpts t a) = Rep a
  to = SubConfigOpts . to
  from (SubConfigOpts x) = from x

instance (GetConfigOptions t, Generic a, GConfigForest1 (Rep a)) => NestedConfig (SubConfigOpts t a) where
  toNestedConfig _ = defaultToNestedConfig (getConfigOptions @t) (Proxy @a)

class GetConfigOptions t where
  getConfigOptions :: ConfigOptions

instance (LabelModifier t) => GetConfigOptions t where
  getConfigOptions = ConfigOptions (getLabelModifier @t)

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
