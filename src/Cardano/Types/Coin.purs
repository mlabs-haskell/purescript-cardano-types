module Cardano.Types.Coin where

import Prelude hiding (zero)

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Types.BigNum (BigNum(BigNum))
import Cardano.Types.BigNum as BigNum
import Data.Generic.Rep (class Generic)
import Data.Lattice (class JoinSemilattice, class MeetSemilattice)
import Data.Maybe (Maybe, fromJust, maybe)
import Data.Newtype (class Newtype, wrap)
import Data.Semiring as Num
import Data.Show.Generic (genericShow)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Safe.Coerce (coerce)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (suchThat)

newtype Coin = Coin BigNum

derive instance Generic Coin _
derive instance Newtype Coin _
derive newtype instance Eq Coin
derive newtype instance Ord Coin
derive newtype instance DecodeAeson Coin
derive newtype instance EncodeAeson Coin
derive newtype instance AsCbor Coin

instance Semigroup Coin where
  append (Coin x) (Coin y) =
    unsafePerformEffect $ maybe (throw "Coin.append: numeric overflow") (pure <<< wrap)
      $ BigNum.add x y

instance Monoid Coin where
  mempty = zero

instance Arbitrary Coin where
  arbitrary = Coin <<< BigNum.fromInt <$> suchThat arbitrary (_ >= Num.zero)

instance Show Coin where
  show (Coin n) = "(Coin.fromStringUnsafe " <> show (BigNum.toString n) <> ")"

instance JoinSemilattice Coin where
  join (Coin c1) (Coin c2) = Coin (max c1 c2)

instance MeetSemilattice Coin where
  meet (Coin c1) (Coin c2) = Coin (min c1 c2)

fromInt :: Int -> Coin
fromInt = Coin <<< BigNum.fromInt

fromString :: String -> Maybe Coin
fromString = map wrap <<< BigNum.fromString

fromStringUnsafe :: Partial => String -> Coin
fromStringUnsafe = wrap <<< fromJust <<< BigNum.fromString

zero :: Coin
zero = Coin BigNum.zero

add :: Coin -> Coin -> Maybe Coin
add = coerce BigNum.add
