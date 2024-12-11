module Cardano.Types.AnchorDataHash
  ( AnchorDataHash(AnchorDataHash)
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError(TypeMismatch), decodeAeson, encodeAeson)
import Cardano.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.Data.Lite (fromBytes, toBytes)
import Cardano.Data.Lite as Csl
import Data.ByteArray (hexToByteArray)
import Data.Either (note)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype AnchorDataHash = AnchorDataHash Csl.AnchorDataHash

derive instance Generic AnchorDataHash _
derive instance Newtype AnchorDataHash _

instance Eq AnchorDataHash where
  eq = eq `on` encodeCbor

instance Ord AnchorDataHash where
  compare = compare `on` encodeCbor

instance Show AnchorDataHash where
  show = genericShow

instance AsCbor AnchorDataHash where
  encodeCbor = wrap <<< toBytes <<< unwrap
  decodeCbor = map wrap <<< fromBytes <<< unwrap

instance EncodeAeson AnchorDataHash where
  encodeAeson = encodeAeson <<< encodeCbor

instance DecodeAeson AnchorDataHash where
  decodeAeson = decodeAeson >=>
    (note (TypeMismatch "AnchorDataHash") <<< (decodeCbor <=< map wrap <<< hexToByteArray))
