module Cardano.Types.Int
  ( Int(Int)
  , add
  , asNegative
  , asPositive
  , fromBigInt
  , fromInt
  , fromString
  , max
  , mul
  , negate
  , newNegative
  , newPositive
  , one
  , sub
  , toBigInt
  , toInt
  , zero
  ) where

import Prelude hiding (zero, sub)

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , encodeAeson
  )
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (Int) as Csl
import Cardano.Serialization.Lib
  ( fromBytes
  , int_asNegative
  , int_asPositive
  , int_new
  , int_newNegative
  , int_toStr
  , toBytes
  )
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum (fromBigInt, fromInt) as BigNum
import Control.Alternative ((<|>))
import Data.Either (note)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Partial.Unsafe (unsafePartial)
import Prelude as Prelude
import Prim (String)
import Prim as Prim

-- | Signed 128-bit integer, -170141183460469231731687303715884105728..170141183460469231731687303715884105727
newtype Int = Int Csl.Int

derive instance Generic Int _
derive instance Newtype Int _

instance Eq Int where
  eq = eq `on` toString

instance Ord Int where
  compare = compare `on` toBigInt

instance Show Int where
  show int = "(fromString " <> show (toString int) <> ")"

instance EncodeAeson Int where
  encodeAeson = encodeAeson <<< toBigInt

instance DecodeAeson Int where
  decodeAeson aeson =
    decodeAeson aeson >>= note (TypeMismatch "Int") <<< fromBigInt

instance AsCbor Int where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

fromBigInt :: BigInt.BigInt -> Maybe Int
fromBigInt bi =
  (newPositive <$> BigNum.fromBigInt bi) <|>
    (newNegative <$> BigNum.fromBigInt (Prelude.negate bi))

toBigInt :: Int -> BigInt.BigInt
toBigInt int =
  -- Assuming every Int can be represented as BigInt
  unsafePartial $ fromJust $ BigInt.fromString $ int_toStr $ unwrap int

zero :: Int
zero = fromInt 0

one :: Int
one = fromInt 1

add :: Int -> Int -> Maybe Int
add = binaryViaBigInt Prelude.add

mul :: Int -> Int -> Maybe Int
mul = binaryViaBigInt Prelude.mul

sub :: Int -> Int -> Maybe Int
sub = binaryViaBigInt Prelude.sub

max :: Int -> Int -> Int
max a b = unsafePartial $ fromJust $ binaryViaBigInt Prelude.max a b

negate :: Int -> Int
negate x = unsafePartial $ fromJust $ sub zero x

binaryViaBigInt :: (BigInt -> BigInt -> BigInt) -> (Int -> Int -> Maybe Int)
binaryViaBigInt f x y = fromBigInt $ f (toBigInt x) (toBigInt y)

fromInt :: Prim.Int -> Int
fromInt n
  | n < 0 = newNegative $ BigNum.fromInt (0 - n)
  | otherwise = newPositive $ BigNum.fromInt n

toInt :: Int -> Maybe Prim.Int
toInt = toBigInt >>> BigInt.toInt

toString :: Int -> String
toString = unwrap >>> int_toStr

fromString :: String -> Maybe Int
fromString = fromBigInt <=< BigInt.fromString

newPositive :: BigNum -> Int
newPositive = unwrap >>> int_new >>> wrap

newNegative :: BigNum -> Int
newNegative = unwrap >>> int_newNegative >>> wrap

asPositive :: Int -> Maybe BigNum
asPositive = unwrap >>> int_asPositive >>> toMaybe >>> map wrap

asNegative :: Int -> Maybe BigNum
asNegative = unwrap >>> int_asNegative >>> toMaybe >>> map wrap
