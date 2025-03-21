module Cardano.Types.Ipv4 where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite (fromBytes, toBytes)
import Cardano.Data.Lite as Cdl
import Cardano.Types.Internal.Helpers (compareViaCdlBytes, eqOrd)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Safe.Coerce (coerce)

newtype Ipv4 = Ipv4 Cdl.Ipv4

instance Eq Ipv4 where
  eq = eqOrd

instance Ord Ipv4 where
  compare = coerce (compareViaCdlBytes :: Cdl.Ipv4 -> Cdl.Ipv4 -> Ordering)

instance AsCbor Ipv4 where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

derive instance Generic Ipv4 _
derive instance Newtype Ipv4 _
derive newtype instance EncodeAeson Ipv4
derive newtype instance DecodeAeson Ipv4

instance Show Ipv4 where
  show = genericShow
