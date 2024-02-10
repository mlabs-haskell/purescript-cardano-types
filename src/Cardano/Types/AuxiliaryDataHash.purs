module Cardano.Types.AuxiliaryDataHash where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.Serialization.Lib (fromBytes, toBytes)
import Cardano.Serialization.Lib as Csl
import Cardano.AsCbor (class AsCbor)
import Cardano.Types.Internal.Helpers (compareViaCslBytes, eqOrd, showFromCbor)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)

newtype AuxiliaryDataHash = AuxiliaryDataHash Csl.AuxiliaryDataHash

derive instance Generic AuxiliaryDataHash _
derive instance Newtype AuxiliaryDataHash _

instance Eq AuxiliaryDataHash where
  eq = eqOrd

instance AsCbor AuxiliaryDataHash where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

derive newtype instance EncodeAeson AuxiliaryDataHash
derive newtype instance DecodeAeson AuxiliaryDataHash

-- This is not newtyped derived because it will be used for ordering a
-- `TransactionInput`, we want lexicographical ordering on the hexstring.
instance Ord AuxiliaryDataHash where
  compare = compareViaCslBytes `on` unwrap

instance Show AuxiliaryDataHash where
  show = unwrap >>> showFromCbor "AuxiliaryDataHash"
