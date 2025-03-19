module Cardano.Types.VRFKeyHash where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , decodeAeson
  , encodeAeson
  )
import Cardano.AsCbor (class AsCbor)
import Cardano.FromData (class FromData, fromData)
import Cardano.Data.Lite (fromBytes, toBytes)
import Cardano.Data.Lite as Csl
import Cardano.ToData (class ToData, toData)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Internal.Helpers (compareViaCslBytes, eqOrd)
import Cardano.Types.PlutusData (PlutusData(Constr))
import Data.ByteArray (byteArrayToHex)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)

newtype VRFKeyHash = VRFKeyHash Csl.VRFKeyHash

derive instance Newtype VRFKeyHash _
derive instance Generic VRFKeyHash _

instance Show VRFKeyHash where
  show = unwrap >>> toBytes >>> byteArrayToHex

instance Eq VRFKeyHash where
  eq = eqOrd

instance Ord VRFKeyHash where
  compare = compareViaCslBytes `on` unwrap

instance FromData VRFKeyHash where
  fromData (Constr n [ bytes ])
    | n == BigNum.zero = VRFKeyHash <$>
        (fromBytes =<< fromData bytes)
  fromData _ = Nothing

instance ToData VRFKeyHash where
  toData (VRFKeyHash th) = Constr BigNum.zero [ toData $ toBytes th ]

instance EncodeAeson VRFKeyHash where
  encodeAeson = unwrap >>> encodeAeson

instance DecodeAeson VRFKeyHash where
  decodeAeson = map wrap <<< decodeAeson

instance AsCbor VRFKeyHash where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap
