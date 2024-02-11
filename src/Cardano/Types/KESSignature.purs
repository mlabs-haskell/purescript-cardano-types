module Cardano.Types.KESSignature
  ( KESSignature(KESSignature)
  ) where

import Prelude

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Internal.Helpers (compareViaCslBytes, eqOrd)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype KESSignature = KESSignature Csl.KESSignature

derive instance Generic KESSignature _
derive instance Newtype KESSignature _

instance Eq KESSignature where
  eq = eqOrd

instance Ord KESSignature where
  compare = compareViaCslBytes `on` unwrap

instance Show KESSignature where
  show = genericShow


instance AsCbor KESSignature where
  encodeCbor = unwrap >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map wrap
