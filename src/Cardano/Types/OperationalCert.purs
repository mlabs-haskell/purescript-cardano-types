module Cardano.Types.OperationalCert where

import Prelude

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Ed25519Signature (Ed25519Signature)
import Cardano.Types.KESVKey (KESVKey)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype OperationalCert = OperationalCert
  { kesvKey :: KESVKey
  , sequenceNumber :: Number
  , kesPeriod :: Number
  , sigma :: Ed25519Signature
  }

derive instance Newtype OperationalCert _
derive instance Generic OperationalCert _
derive instance Eq OperationalCert
derive instance Ord OperationalCert

instance Show OperationalCert where
  show = genericShow

instance AsCbor OperationalCert where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

fromCsl :: Csl.OperationalCert -> OperationalCert
fromCsl input =
  let
    kesvKey = wrap $ Csl.operationalCert_hotVkey input
    sequenceNumber = Csl.operationalCert_sequenceNumber input
    kesPeriod = Csl.operationalCert_kesPeriod input
    sigma = wrap $ Csl.operationalCert_sigma input
  in
    OperationalCert {kesvKey, sequenceNumber, kesPeriod, sigma}

toCsl :: OperationalCert -> Csl.OperationalCert
toCsl (OperationalCert input) =
  let
    kesvKey = unwrap $ input.kesvKey
    sequenceNumber = input.sequenceNumber
    kesPeriod = input.kesPeriod
    sigma = unwrap $ input.sigma
  in
    Csl.operationalCert_new kesvKey sequenceNumber kesPeriod sigma
