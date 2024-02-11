module Cardano.Types.KESVKey
  ( KESVKey(KESVKey)
  , fromBech32
  , toBech32
  ) where

import Prelude

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Bech32String (Bech32String)
import Cardano.Types.Internal.Helpers (compareViaCslBytes, eqOrd)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)

newtype KESVKey = KESVKey Csl.KESVKey

derive instance Generic KESVKey _
derive instance Newtype KESVKey _

instance Eq KESVKey where
  eq = eqOrd

instance Ord KESVKey where
  compare = compareViaCslBytes `on` unwrap

instance Show KESVKey where
  show = genericShow

instance AsCbor KESVKey where
  encodeCbor = unwrap >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map wrap

fromBech32 :: Bech32String -> Maybe KESVKey
fromBech32 = Csl.kesvKey_fromBech32 >>> toMaybe >>> map wrap

toBech32 :: String -> KESVKey -> Bech32String
toBech32 prefix = unwrap >>> flip Csl.kesvKey_toBech32 prefix
