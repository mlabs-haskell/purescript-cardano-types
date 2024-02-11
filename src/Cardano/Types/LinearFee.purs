module Cardano.Types.LinearFee
  ( LinearFee(..)
  , fromCsl
  , toCsl
  )
  where

import Prelude

import Cardano.Serialization.Lib as Csl
import Cardano.Types.BigNum (BigNum)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype LinearFee = LinearFee
  { constant :: BigNum
  , coefficient :: BigNum
  }

derive instance Newtype LinearFee _
derive instance Generic LinearFee _
derive instance Eq LinearFee
derive instance Ord LinearFee

instance Show LinearFee where
  show = genericShow

fromCsl :: Csl.LinearFee -> LinearFee
fromCsl input =
  let
    constant = wrap $ Csl.linearFee_constant input
    coefficient = wrap $ Csl.linearFee_coefficient input
  in
    LinearFee { constant, coefficient }

toCsl :: LinearFee -> Csl.LinearFee
toCsl (LinearFee input) =
  Csl.linearFee_new
    (unwrap $ input.constant )
    (unwrap $ input.coefficient)
