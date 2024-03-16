module Cardano.Types.BigNum
  ( BigNum(BigNum)
  , add
  , divFloor
  , fromBigInt
  , fromInt
  , fromString
  , fromStringUnsafe
  , fromUInt
  , maxValue
  , mul
  , sub
  , one
  , toBigInt
  , toInt
  , toString
  , toUInt
  , zero
  , max
  , abs
  ) where

import Prelude hiding (sub, add, one, zero, max)

import Aeson (JsonDecodeError(TypeMismatch)) as Aeson
import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (bigNum_checkedAdd, bigNum_checkedMul, bigNum_checkedSub, bigNum_compare, bigNum_divFloor, bigNum_fromStr, bigNum_max, bigNum_maxValue, bigNum_one, bigNum_toStr, bigNum_zero, fromBytes, toBytes)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Internal.Helpers (eqOrd)
import Data.Array.NonEmpty as NA
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe, fromJust, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.UInt (UInt)
import Data.UInt (fromInt, fromString, toString) as UInt
import JS.BigInt (BigInt)
import JS.BigInt (fromString, toString) as BigInt
import Partial.Unsafe (unsafePartial)
import Safe.Coerce (coerce)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt, oneOf)

-- | Unisigned 64-bit integer, 0..18446744073709551615
newtype BigNum = BigNum Csl.BigNum

derive instance Generic BigNum _
derive instance Newtype BigNum _

instance Eq BigNum where
  eq = eqOrd

instance Ord BigNum where
  compare (BigNum lhs) (BigNum rhs) =
    case bigNum_compare lhs rhs of
      1.0 -> GT
      0.0 -> EQ
      _ -> LT

instance Show BigNum where
  show bn = "fromString \"" <> toString bn <> "\""

instance DecodeAeson BigNum where
  decodeAeson =
    note (Aeson.TypeMismatch "Couldn't convert `BigInt` to `BigNum`")
      <<< fromBigInt <=< decodeAeson

instance EncodeAeson BigNum where
  encodeAeson = encodeAeson <<< toBigInt

instance AsCbor BigNum where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

instance Arbitrary BigNum where
  arbitrary = oneOf $ unsafePartial $ fromJust $ NA.fromFoldable
    [ fromInt <$> chooseInt 0 top
    , pure $ fromInt 0
    , pure $ unsafePartial $ fromJust $ fromString "18446744073709551615"
    ]

-- Semiring cannot be implemented, because add and mul returns Maybe BigNum

one :: BigNum
one = BigNum bigNum_one

zero :: BigNum
zero = BigNum bigNum_zero

add :: BigNum -> BigNum -> Maybe BigNum
add (BigNum a) (BigNum b) = coerce $ toMaybe $ bigNum_checkedAdd a b

mul :: BigNum -> BigNum -> Maybe BigNum
mul (BigNum a) (BigNum b) = coerce $ toMaybe $ bigNum_checkedMul a b

sub :: BigNum -> BigNum -> Maybe BigNum
sub (BigNum a) (BigNum b) = coerce $ toMaybe $ bigNum_checkedSub a b

max :: BigNum -> BigNum -> BigNum
max = coerce bigNum_max

abs :: BigNum -> BigNum
abs n = max n (fromMaybe zero $ sub zero n)

divFloor :: BigNum -> BigNum -> BigNum
divFloor (BigNum a) (BigNum b) = BigNum $ bigNum_divFloor a b

fromBigInt :: BigInt -> Maybe BigNum
fromBigInt = fromString <<< BigInt.toString

toBigInt :: BigNum -> BigInt
toBigInt =
  -- Converting uint64 to an arbitrary length integer should never fail.
  unsafePartial fromJust <<< BigInt.fromString <<< toString

toInt :: BigNum -> Maybe Int
toInt = Int.fromString <<< toString

-- | Converts an `Int` to a `BigNum` turning negative `Int`s into `BigNum`s
-- | in range from `2^31` to `2^32-1`.
fromInt :: Int -> BigNum
fromInt =
  -- Converting `UInt` (u32) to a `BigNum` (u64) should never fail.
  fromStringUnsafe <<< UInt.toString <<< UInt.fromInt

toString :: BigNum -> String
toString = unwrap >>> bigNum_toStr

fromString :: String -> Maybe BigNum
fromString = map wrap <<< toMaybe <<< bigNum_fromStr

fromStringUnsafe :: String -> BigNum
fromStringUnsafe = unsafePartial fromJust <<< fromString

maxValue :: BigNum
maxValue = BigNum bigNum_maxValue

fromUInt :: UInt -> BigNum
fromUInt = fromStringUnsafe <<< UInt.toString

toUInt :: BigNum -> Maybe UInt
toUInt = toString >>> UInt.fromString
