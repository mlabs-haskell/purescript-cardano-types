module Cardano.Types.Redeemer where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
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
  encodeAeson = toCdl >>> encodeAeson

instance DecodeAeson Redeemer where
  decodeAeson = map fromCdl <<< decodeAeson

instance Show Redeemer where
  show = genericShow

instance AsCbor Redeemer where
  encodeCbor = toCdl >>> Cdl.toBytes >>> wrap
  decodeCbor = unwrap >>> Cdl.fromBytes >>> map fromCdl

fromCdl :: Cdl.Redeemer -> Redeemer
fromCdl input =
  let
    tag = RedeemerTag.fromCdl $ Cdl.redeemer_tag input
    index = wrap $ Cdl.redeemer_index input
    d = PlutusData.fromCdl $ Cdl.redeemer_data input
    exUnits = ExUnits.fromCdl $ Cdl.redeemer_exUnits input
  in
    Redeemer { tag, index, data: wrap d, exUnits }

toCdl :: Redeemer -> Cdl.Redeemer
toCdl (Redeemer input) =
  let
    tag = RedeemerTag.toCdl $ input.tag
    index = unwrap input.index
    d = PlutusData.toCdl $ unwrap $ input.data
    exUnits = ExUnits.toCdl $ input.exUnits
  in
    Cdl.redeemer_new tag index d exUnits
