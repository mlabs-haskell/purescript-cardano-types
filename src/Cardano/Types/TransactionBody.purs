module Cardano.Types.TransactionBody
  ( TransactionBody(..)
  , toCsl
  , fromCsl
  ) where

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Internal.Helpers (compareViaCslBytes, eqOrd)
import Control.Apply (map)
import Data.Eq (class Eq)
import Data.Function (on, ($), (>>>))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Ord (class Ord)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

-- TODO better representation?
newtype TransactionBody = TransactionBody Csl.TransactionBody

derive instance Newtype TransactionBody _
derive instance Generic TransactionBody _

instance Eq TransactionBody where
  eq = eqOrd

instance Ord TransactionBody where
  compare = compareViaCslBytes `on` unwrap

instance Show TransactionBody where
    show = genericShow

instance AsCbor TransactionBody where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

fromCsl :: Csl.TransactionBody -> TransactionBody
fromCsl =
  unsafePerformEffect $ throw "TransactionBody.fromCsl: not implemented"

toCsl :: TransactionBody -> Csl.TransactionBody
toCsl =
  unsafePerformEffect $ throw "TransactionBody.fromCsl: not implemented"
