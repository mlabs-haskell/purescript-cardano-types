module Cardano.Types.BootstrapWitness
  ( BootstrapWitness(BootstrapWitness)
  , fromCsl
  , toCsl
  ) where

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (bootstrapWitness_attributes, bootstrapWitness_chainCode, bootstrapWitness_new, bootstrapWitness_signature, bootstrapWitness_vkey)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Ed25519Signature (Ed25519Signature)
import Cardano.Types.Vkey (Vkey)
import Cardano.Types.Vkey as Vkey
import Control.Apply (map)
import Data.ByteArray (ByteArray)
import Data.Eq (class Eq)
import Data.Function (($), (>>>))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)

newtype BootstrapWitness = BootstrapWitness
  { vkey :: Vkey
  , signature :: Ed25519Signature
  , chainCode :: ByteArray
  , attributes :: ByteArray
  }

derive instance Newtype BootstrapWitness _
derive instance Eq BootstrapWitness
derive instance Generic BootstrapWitness _

instance Show BootstrapWitness where
  show = genericShow

instance AsCbor BootstrapWitness where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

fromCsl :: Csl.BootstrapWitness -> BootstrapWitness
fromCsl bw =
  let
    vkey = Vkey.fromCsl $ bootstrapWitness_vkey bw
    signature = wrap $ bootstrapWitness_signature bw
    chainCode = bootstrapWitness_chainCode bw
    attributes = bootstrapWitness_attributes bw
  in
    wrap { vkey, signature, chainCode, attributes }

toCsl :: BootstrapWitness -> Csl.BootstrapWitness
toCsl (BootstrapWitness { vkey, signature, chainCode, attributes }) =
  bootstrapWitness_new (Vkey.toCsl vkey) (unwrap signature) chainCode attributes
