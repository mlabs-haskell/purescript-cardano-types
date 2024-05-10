module Cardano.Types.RedeemerDatum where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Types.PlutusData (PlutusData)
import Cardano.Types.PlutusData as PlutusData
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import Test.QuickCheck (class Arbitrary)

newtype RedeemerDatum = RedeemerDatum PlutusData

derive instance Eq RedeemerDatum
derive instance Ord RedeemerDatum
derive instance Generic RedeemerDatum _
derive instance Newtype RedeemerDatum _

derive newtype instance EncodeAeson RedeemerDatum
derive newtype instance DecodeAeson RedeemerDatum
derive newtype instance AsCbor RedeemerDatum
derive newtype instance Arbitrary RedeemerDatum

instance Show RedeemerDatum where
  show x = genericShow x

unit :: RedeemerDatum
unit = wrap PlutusData.unit
