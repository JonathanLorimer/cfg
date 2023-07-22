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
--
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
