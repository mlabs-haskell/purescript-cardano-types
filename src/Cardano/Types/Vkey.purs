module Cardano.Types.Vkey where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
import Cardano.Types.PublicKey (PublicKey(PublicKey))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype Vkey = Vkey PublicKey

derive instance Generic Vkey _
derive instance Newtype Vkey _
derive newtype instance Eq Vkey
derive newtype instance Ord Vkey
derive newtype instance EncodeAeson Vkey
derive newtype instance DecodeAeson Vkey

instance Show Vkey where
  show = genericShow

fromCdl :: Cdl.Vkey -> Vkey
fromCdl = Cdl.vkey_publicKey >>> wrap >>> wrap

toCdl :: Vkey -> Cdl.Vkey
toCdl = unwrap >>> unwrap >>> Cdl.vkey_new

instance AsCbor Vkey where
  encodeCbor = toCdl >>> Cdl.toBytes >>> wrap
  decodeCbor = unwrap >>> Cdl.fromBytes >>> map fromCdl
