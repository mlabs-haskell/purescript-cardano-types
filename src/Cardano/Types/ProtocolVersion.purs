module Cardano.Types.ProtocolVersion where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)

newtype ProtocolVersion = ProtocolVersion
  { major :: Int
  , minor :: Int
  }

derive instance Newtype ProtocolVersion _
derive instance Eq ProtocolVersion
derive instance Ord ProtocolVersion
derive instance Generic ProtocolVersion _
derive newtype instance EncodeAeson ProtocolVersion
derive newtype instance DecodeAeson ProtocolVersion

instance Show ProtocolVersion where
  show = genericShow

instance AsCbor ProtocolVersion where
  encodeCbor = toCdl >>> Cdl.toBytes >>> wrap
  decodeCbor = unwrap >>> Cdl.fromBytes >>> map fromCdl

toCdl :: ProtocolVersion -> Cdl.ProtocolVersion
toCdl (ProtocolVersion { major, minor }) = Cdl.protocolVersion_new (Int.toNumber major) (Int.toNumber minor)

fromCdl :: Cdl.ProtocolVersion -> ProtocolVersion
fromCdl csl = ProtocolVersion
  { major: unsafePartial $ fromJust $ Int.fromNumber $ Cdl.protocolVersion_major csl
  , minor: unsafePartial $ fromJust $ Int.fromNumber $ Cdl.protocolVersion_minor csl
  }
