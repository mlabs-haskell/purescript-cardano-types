module Cardano.Types.Redeemer where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Csl
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.ExUnits (ExUnits)
import Cardano.Types.ExUnits as ExUnits
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.RedeemerDatum (RedeemerDatum)
import Cardano.Types.RedeemerTag (RedeemerTag)
import Cardano.Types.RedeemerTag as RedeemerTag
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype Redeemer = Redeemer
  { tag :: RedeemerTag
  , index :: BigNum
  , data :: RedeemerDatum
  , exUnits :: ExUnits
  }

derive instance Newtype Redeemer _
derive instance Generic Redeemer _
derive instance Eq Redeemer
derive instance Ord Redeemer

instance EncodeAeson Redeemer where
  encodeAeson = toCsl >>> encodeAeson

instance DecodeAeson Redeemer where
  decodeAeson = map fromCsl <<< decodeAeson

instance Show Redeemer where
  show = genericShow

instance AsCbor Redeemer where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

fromCsl :: Csl.Redeemer -> Redeemer
fromCsl input =
  let
    tag = RedeemerTag.fromCsl $ Csl.redeemer_tag input
    index = wrap $ Csl.redeemer_index input
    d = PlutusData.fromCsl $ Csl.redeemer_data input
    exUnits = ExUnits.fromCsl $ Csl.redeemer_exUnits input
  in
    Redeemer { tag, index, data: wrap d, exUnits }

toCsl :: Redeemer -> Csl.Redeemer
toCsl (Redeemer input) =
  let
    tag = RedeemerTag.toCsl $ input.tag
    index = unwrap input.index
    d = PlutusData.toCsl $ unwrap $ input.data
    exUnits = ExUnits.toCsl $ input.exUnits
  in
    Csl.redeemer_new tag index d exUnits
