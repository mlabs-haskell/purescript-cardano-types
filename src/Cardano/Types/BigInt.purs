module Cardano.Types.BigInt
  ( module X
  , fromCsl
  , toCsl
  , divCeil
  , CborBigInt(..)
  ) where

import Prelude

import Data.EuclideanRing (class CommutativeRing, class EuclideanRing, class Semiring)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (bigInt_divCeil, bigInt_fromStr, bigInt_toStr)
import Cardano.Serialization.Lib as Csl
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import JS.BigInt (BigInt, Parity(Even, Odd), Radix, and, asIntN, asUintN, binary, decimal, even, fromInt, fromNumber, fromString, fromStringAs, fromTLInt, hexadecimal, not, octal, odd, or, parity, pow, shl, shr, toInt, toNumber, toString, toStringAs, xor) as X
import Partial.Unsafe (unsafePartial)

newtype CborBigInt = CborBigInt BigInt

derive newtype instance Eq CborBigInt
derive newtype instance Ord CborBigInt
derive newtype instance Show CborBigInt
derive newtype instance Semiring CborBigInt
derive newtype instance Ring CborBigInt
derive newtype instance CommutativeRing CborBigInt
derive newtype instance EuclideanRing CborBigInt

derive instance Generic CborBigInt _
derive instance Newtype CborBigInt _

toCsl :: BigInt -> Csl.BigInt
toCsl bi = unsafePartial $ fromJust $ toMaybe $ bigInt_fromStr $
  BigInt.toString bi

fromCsl :: Csl.BigInt -> BigInt
fromCsl bi = unsafePartial $ fromJust $ BigInt.fromString $ bigInt_toStr bi

divCeil :: BigInt -> BigInt -> BigInt
divCeil a b = fromCsl $ bigInt_divCeil (toCsl a) (toCsl b)

instance AsCbor CborBigInt where
  encodeCbor = unwrap >>> toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map (fromCsl >>> wrap)
