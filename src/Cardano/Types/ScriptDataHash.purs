module Cardano.Types.ScriptDataHash where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite (fromBytes, toBytes)
import Cardano.Data.Lite as Cdl
import Cardano.Types.Bech32String (Bech32String)
import Cardano.Types.Internal.Helpers (compareViaCdlBytes, eqOrd, showFromCbor)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)

newtype ScriptDataHash = ScriptDataHash Cdl.ScriptDataHash

derive instance Newtype ScriptDataHash _
derive instance Generic ScriptDataHash _

instance Eq ScriptDataHash where
  eq = eqOrd

instance AsCbor ScriptDataHash where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

derive newtype instance DecodeAeson ScriptDataHash
derive newtype instance EncodeAeson ScriptDataHash

-- This is not newtyped derived because it will be used for ordering a
-- `TransactionInput`, we want lexicographical ordering on the hexstring.
instance Ord ScriptDataHash where
  compare = compareViaCdlBytes `on` unwrap

instance Show ScriptDataHash where
  show = unwrap >>> showFromCbor "ScriptDataHash"

toBech32 :: String -> ScriptDataHash -> Bech32String
toBech32 s = unwrap >>> flip Cdl.scriptDataHash_toBech32 s

fromBech32 :: Bech32String -> Maybe ScriptDataHash
fromBech32 = Cdl.scriptDataHash_fromBech32 >>> toMaybe >>> map wrap
