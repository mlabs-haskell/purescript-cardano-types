module Cardano.Types.TransactionWitnessSet
  ( TransactionWitnessSet(TransactionWitnessSet)
  , fromCsl
  , toCsl
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

-- TODO:
newtype TransactionWitnessSet = TransactionWitnessSet Csl.TransactionWitnessSet

derive instance Newtype TransactionWitnessSet _
derive instance Generic TransactionWitnessSet _

instance Eq TransactionWitnessSet where
  eq = eqOrd

instance Ord TransactionWitnessSet where
  compare = compareViaCslBytes `on` unwrap

instance Show TransactionWitnessSet where
  show = genericShow

instance AsCbor TransactionWitnessSet where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

fromCsl :: Csl.TransactionWitnessSet -> TransactionWitnessSet
fromCsl =
  unsafePerformEffect $ throw "TransactionWitnessSet.fromCsl: not implemented"

toCsl :: TransactionWitnessSet -> Csl.TransactionWitnessSet
toCsl =
  unsafePerformEffect $ throw "TransactionWitnessSet.fromCsl: not implemented"
