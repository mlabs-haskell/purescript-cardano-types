-- naming in sync with CSL
module Cardano.Types.Vkeywitness where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite (vkeywitness_new, vkeywitness_signature, vkeywitness_vkey)
import Cardano.Data.Lite as Cdl
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
  encodeCbor = toCdl >>> Cdl.toBytes >>> wrap
  decodeCbor = unwrap >>> Cdl.fromBytes >>> map fromCdl

toCdl :: Vkeywitness -> Cdl.Vkeywitness
toCdl (Vkeywitness { vkey, signature }) =
  vkeywitness_new (Vkey.toCdl vkey) (unwrap signature)

fromCdl :: Cdl.Vkeywitness -> Vkeywitness
fromCdl vkw =
  let
    vkey = Vkey.fromCdl $ vkeywitness_vkey vkw
    signature = wrap $ vkeywitness_signature vkw
  in
    Vkeywitness
      { vkey
      , signature
      }
