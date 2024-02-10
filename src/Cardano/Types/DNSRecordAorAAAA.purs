module Cardano.Crypto.Csl.Types.DNSRecordAorAAAA
  ( DNSRecordAorAAAA(..)
  ) where

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Control.Apply (map)
import Data.Eq (class Eq)
import Data.Function ((>>>))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)

newtype DNSRecordAorAAAA = DNSRecordAorAAAA { record :: String }


derive instance Newtype DNSRecordAorAAAA _
derive instance Eq DNSRecordAorAAAA
derive instance Generic DNSRecordAorAAAA _


instance Show DNSRecordAorAAAA where
    show = genericShow

instance AsCbor DNSRecordAorAAAA where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

fromCsl :: Csl.DNSRecordAorAAAA -> DNSRecordAorAAAA
fromCsl = Csl.dnsRecordAorAAAA_record >>> {record: _} >>> DNSRecordAorAAAA

toCsl :: DNSRecordAorAAAA -> Csl.DNSRecordAorAAAA
toCsl (DNSRecordAorAAAA {record}) = Csl.dnsRecordAorAAAA_new record
