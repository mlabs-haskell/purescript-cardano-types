module Cardano.AsCbor where

import Prelude

import Cardano.Data.Lite (bigInt_fromStr, bigInt_toStr, fromBytes, toBytes)
import Cardano.Data.Lite as Cdl
import Cardano.Types.CborBytes (CborBytes)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Partial.Unsafe (unsafePartial)

-- | A typeclass for Cardano domain types that can encoded to and decoded from `CborBytes`
class AsCbor a where
  encodeCbor :: a -> CborBytes
  decodeCbor :: CborBytes -> Maybe a

-- we have to put it here due to absence of orphan instances in PS
instance AsCbor BigInt where
  encodeCbor = toCdl >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map fromCdl

toCdl :: BigInt -> Cdl.BigInt
toCdl bi = unsafePartial $ fromJust $ toMaybe $ bigInt_fromStr $
  BigInt.toString bi

fromCdl :: Cdl.BigInt -> BigInt
fromCdl bi = unsafePartial $ fromJust $ BigInt.fromString $ bigInt_toStr bi
