module Cardano.Types.Int
  ( Int(Int)
  , newPositive
  , newNegative
  , fromBigInt
  , toBigInt
  , fromInt
  , toInt
  , fromString
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , encodeAeson
  )
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (Int) as Csl
import Cardano.Serialization.Lib (fromBytes, toBytes, int_new, int_newNegative, int_toStr)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum (fromBigInt, fromInt) as BigNum
import Control.Alternative ((<|>))
import Data.Either (note)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import JS.BigInt as BigInt
import Partial.Unsafe (unsafePartial)
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
    (newNegative <$> BigNum.fromBigInt (negate bi))

toBigInt :: Int -> BigInt.BigInt
toBigInt int =
  -- Assuming every Int can be represented as BigInt
  unsafePartial $ fromJust $ BigInt.fromString $ int_toStr $ unwrap int

fromInt :: Prim.Int -> Int
fromInt n
  | n < 0 = newNegative $ BigNum.fromInt n
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
