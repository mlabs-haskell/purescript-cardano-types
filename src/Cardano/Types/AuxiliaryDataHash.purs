module Cardano.Types.AuxiliaryDataHash where

import Prelude

import Aeson (class EncodeAeson)
import Cardano.AsCbor (class AsCbor, encodeCbor)
import Cardano.Serialization.Lib (fromBytes, toBytes)
import Cardano.Serialization.Lib as Csl
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype AuxiliaryDataHash = AuxiliaryDataHash Csl.AuxiliaryDataHash

derive instance Newtype AuxiliaryDataHash _
derive instance Generic AuxiliaryDataHash _

instance Eq AuxiliaryDataHash where
  eq = eq `on` encodeCbor

instance Ord AuxiliaryDataHash where
  compare = compare `on` encodeCbor

instance Show AuxiliaryDataHash where
  show = genericShow

instance AsCbor AuxiliaryDataHash where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

derive newtype instance EncodeAeson AuxiliaryDataHash
