-- We reuse JS.BigInt.BigInt here. Csl.BigInt is not used at runtime at
-- all, except for divCeil
module Cardano.Types.BigInt
  ( module X
  , fromCsl
  , toCsl
  , divCeil
  ) where

import Prelude

import Cardano.Data.Lite (bigInt_divCeil, bigInt_fromStr, bigInt_toStr)
import Cardano.Data.Lite as Csl
import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)
import JS.BigInt (BigInt)
import JS.BigInt
  ( BigInt
  , Parity(Even, Odd)
  , Radix
  , and
  , asIntN
  , asUintN
  , binary
  , decimal
  , even
  , fromInt
  , fromNumber
  , fromString
  , fromStringAs
  , fromTLInt
  , hexadecimal
  , not
  , octal
  , odd
  , or
  , parity
  , pow
  , shl
  , shr
  , toInt
  , toNumber
  , toString
  , toStringAs
  , xor
  ) as X
import JS.BigInt as BigInt
import Partial.Unsafe (unsafePartial)

toCsl :: BigInt -> Csl.BigInt
toCsl bi = unsafePartial $ fromJust $ toMaybe $ bigInt_fromStr $
  BigInt.toString bi

fromCsl :: Csl.BigInt -> BigInt
fromCsl bi = unsafePartial $ fromJust $ BigInt.fromString $ bigInt_toStr bi

divCeil :: BigInt -> BigInt -> BigInt
divCeil a b = fromCsl $ bigInt_divCeil (toCsl a) (toCsl b)
