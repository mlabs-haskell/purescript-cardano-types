module Cardano.Types.DNSRecordSRV
  ( DNSRecordSRV(..)
  , fromCsl
  , toCsl
  ) where

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Control.Apply (map)
import Data.Eq (class Eq)
import Data.Function ((>>>))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Ord (class Ord)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)

newtype DNSRecordSRV = DNSRecordSRV { record :: String }

derive instance Newtype DNSRecordSRV _
derive instance Eq DNSRecordSRV
derive instance Generic DNSRecordSRV _
derive instance Ord DNSRecordSRV

instance Show DNSRecordSRV where
    show = genericShow

instance AsCbor DNSRecordSRV where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

fromCsl :: Csl.DNSRecordSRV -> DNSRecordSRV
fromCsl = Csl.dnsRecordSRV_record >>> {record: _} >>> DNSRecordSRV

toCsl :: DNSRecordSRV -> Csl.DNSRecordSRV
toCsl (DNSRecordSRV {record}) = Csl.dnsRecordSRV_new record
