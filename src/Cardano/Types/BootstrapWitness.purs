module Cardano.Types.BootstrapWitness
  ( BootstrapWitness(BootstrapWitness)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite
  ( bootstrapWitness_attributes
  , bootstrapWitness_chainCode
  , bootstrapWitness_new
  , bootstrapWitness_signature
  , bootstrapWitness_vkey
  )
import Cardano.Data.Lite as Cdl
import Cardano.Types.Ed25519Signature (Ed25519Signature)
import Cardano.Types.Vkey (Vkey)
import Cardano.Types.Vkey as Vkey
import Data.ByteArray (ByteArray)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype BootstrapWitness = BootstrapWitness
  { vkey :: Vkey
  , signature :: Ed25519Signature
  , chainCode :: ByteArray
  , attributes :: ByteArray
  }

derive instance Newtype BootstrapWitness _
derive instance Eq BootstrapWitness
derive instance Ord BootstrapWitness
derive instance Generic BootstrapWitness _

instance Show BootstrapWitness where
  show = genericShow

instance AsCbor BootstrapWitness where
  encodeCbor = toCdl >>> Cdl.toBytes >>> wrap
  decodeCbor = unwrap >>> Cdl.fromBytes >>> map fromCdl

instance EncodeAeson BootstrapWitness where
  encodeAeson = toCdl >>> encodeAeson

instance DecodeAeson BootstrapWitness where
  decodeAeson = map fromCdl <<< decodeAeson

fromCdl :: Cdl.BootstrapWitness -> BootstrapWitness
fromCdl bw =
  let
    vkey = Vkey.fromCdl $ bootstrapWitness_vkey bw
    signature = wrap $ bootstrapWitness_signature bw
    chainCode = bootstrapWitness_chainCode bw
    attributes = bootstrapWitness_attributes bw
  in
    wrap { vkey, signature, chainCode, attributes }

toCdl :: BootstrapWitness -> Cdl.BootstrapWitness
toCdl (BootstrapWitness { vkey, signature, chainCode, attributes }) =
  bootstrapWitness_new (Vkey.toCdl vkey) (unwrap signature) chainCode attributes
