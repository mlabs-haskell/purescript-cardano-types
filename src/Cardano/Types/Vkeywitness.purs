-- naming in sync with CSL
module Cardano.Types.Vkeywitness where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite (vkeywitness_new, vkeywitness_signature, vkeywitness_vkey)
import Cardano.Data.Lite as Csl
import Cardano.Types.Ed25519Signature (Ed25519Signature)
import Cardano.Types.Vkey (Vkey)
import Cardano.Types.Vkey as Vkey
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype Vkeywitness = Vkeywitness
  { vkey :: Vkey
  , signature :: Ed25519Signature
  }

derive instance Generic Vkeywitness _
derive instance Newtype Vkeywitness _
derive instance Eq Vkeywitness
derive instance Ord Vkeywitness
derive newtype instance EncodeAeson Vkeywitness
derive newtype instance DecodeAeson Vkeywitness

instance Show Vkeywitness where
  show = genericShow

instance AsCbor Vkeywitness where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

toCsl :: Vkeywitness -> Csl.Vkeywitness
toCsl (Vkeywitness { vkey, signature }) =
  vkeywitness_new (Vkey.toCsl vkey) (unwrap signature)

fromCsl :: Csl.Vkeywitness -> Vkeywitness
fromCsl vkw =
  let
    vkey = Vkey.fromCsl $ vkeywitness_vkey vkw
    signature = wrap $ vkeywitness_signature vkw
  in
    Vkeywitness
      { vkey
      , signature
      }
