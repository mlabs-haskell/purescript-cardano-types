module Cardano.Types.Ipv6 where

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

newtype Ipv6 = Ipv6 Cdl.Ipv6

instance Eq Ipv6 where
  eq = eqOrd

instance Ord Ipv6 where
  compare = coerce (compareViaCdlBytes :: Cdl.Ipv6 -> Cdl.Ipv6 -> Ordering)

instance AsCbor Ipv6 where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

derive instance Generic Ipv6 _
derive instance Newtype Ipv6 _
derive newtype instance EncodeAeson Ipv6
derive newtype instance DecodeAeson Ipv6

instance Show Ipv6 where
  show = genericShow
