module Cfg.Deriving.Assert where

import GHC.TypeError (TypeError, ErrorMessage (..))
import GHC.Generics
import Data.Kind (Type)
import GHC.Base (Constraint)

class Assert (pred :: Bool) (msg :: ErrorMessage)
instance Assert 'True msg
instance TypeError msg ~ '() => Assert 'False msg

type family IsTopLevelRecord f where
  IsTopLevelRecord V1 = 'False
  IsTopLevelRecord U1 = 'False
  IsTopLevelRecord (K1 i c) = 'False
  IsTopLevelRecord (M1 i c f) = IsTopLevelRecord f
  IsTopLevelRecord (f :*: g) = 'True
  IsTopLevelRecord (f :+: g) = 'False

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

