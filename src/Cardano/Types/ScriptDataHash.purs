module Cardano.Types.ScriptDataHash where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (fromBytes, toBytes)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Bech32String (Bech32String)
import Cardano.Types.Internal.Helpers (compareViaCslBytes, eqOrd, showFromCbor)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)

newtype ScriptDataHash = ScriptDataHash Csl.ScriptDataHash

derive instance Generic ScriptDataHash _
derive instance Newtype ScriptDataHash _

instance Eq ScriptDataHash where
  eq = eqOrd

instance AsCbor ScriptDataHash where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

derive newtype instance EncodeAeson ScriptDataHash
derive newtype instance DecodeAeson ScriptDataHash

-- This is not newtyped derived because it will be used for ordering a
-- `TransactionInput`, we want lexicographical ordering on the hexstring.
instance Ord ScriptDataHash where
  compare = compareViaCslBytes `on` unwrap

instance Show ScriptDataHash where
  show = unwrap >>> showFromCbor "ScriptDataHash"

toBech32 :: String -> ScriptDataHash -> Bech32String
toBech32 s = unwrap >>> flip Csl.scriptDataHash_toBech32 s

fromBech32 :: Bech32String -> Maybe ScriptDataHash
fromBech32 = Csl.scriptDataHash_fromBech32 >>> toMaybe >>> map wrap
