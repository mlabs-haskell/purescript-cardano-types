module Cardano.Types.PrivateKey where

import Prelude

import Aeson (class EncodeAeson, encodeAeson)
import Cardano.Serialization.Lib
  ( privateKey_asBytes
  , privateKey_toBech32
  , privateKey_toPublic
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Bech32String (Bech32String)
import Cardano.Types.Internal.Helpers (eqOrd)
import Cardano.Types.PublicKey (PublicKey)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)

newtype PrivateKey = PrivateKey Csl.PrivateKey

derive instance Generic PrivateKey _
derive instance Newtype PrivateKey _

instance Eq PrivateKey where
  eq = eqOrd

instance Ord PrivateKey where
  compare = compare `on` (unwrap >>> privateKey_asBytes)

instance EncodeAeson PrivateKey where
  encodeAeson = unwrap >>> privateKey_asBytes >>> encodeAeson

instance Show PrivateKey where
  show _ = "(PrivateKey <HIDDEN>)"

toBech32 :: PrivateKey -> Bech32String
toBech32 = unwrap >>> privateKey_toBech32

toPublicKey :: PrivateKey -> PublicKey
toPublicKey = unwrap >>> privateKey_toPublic >>> wrap
