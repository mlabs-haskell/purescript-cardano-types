module Cardano.Types.ProtocolVersion where

import Prelude

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype ProtocolVersion = ProtocolVersion
  { major :: Number
  , minor :: Number
  }

derive instance Newtype ProtocolVersion _
derive instance Eq ProtocolVersion
derive instance Ord ProtocolVersion
derive instance Generic ProtocolVersion _

instance Show ProtocolVersion where
  show = genericShow

instance AsCbor ProtocolVersion where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

toCsl :: ProtocolVersion -> Csl.ProtocolVersion
toCsl (ProtocolVersion { major, minor }) = Csl.protocolVersion_new major minor

fromCsl :: Csl.ProtocolVersion -> ProtocolVersion
fromCsl csl = ProtocolVersion
  { major: Csl.protocolVersion_major csl
  , minor: Csl.protocolVersion_minor csl
  }

