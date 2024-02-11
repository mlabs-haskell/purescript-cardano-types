module Cardano.Types.MultiHostName where

import Prelude

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.DNSRecordSRV (DNSRecordSRV(..))
import Cardano.Types.DNSRecordSRV as DNSRecordSRV
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype MultiHostName = MultiHostName DNSRecordSRV

derive instance Generic MultiHostName _
derive instance Newtype MultiHostName _
derive instance Eq MultiHostName
derive instance Ord MultiHostName

instance Show MultiHostName where
  show = genericShow

instance AsCbor MultiHostName where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

toCsl :: MultiHostName -> Csl.MultiHostName
toCsl x = Csl.multiHostName_new (DNSRecordSRV.toCsl (unwrap x))

fromCsl :: Csl.MultiHostName -> MultiHostName
fromCsl x = wrap (DNSRecordSRV.fromCsl (Csl.multiHostName_dnsName x))
