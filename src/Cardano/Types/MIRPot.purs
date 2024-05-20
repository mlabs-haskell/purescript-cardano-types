module Cardano.Types.MIRPot
  ( MIRPot(Reserves, Treasury)
  , fromCsl
  , fromInt
  , toCsl
  , toInt
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError(TypeMismatch), decodeAeson, encodeAeson)
import Cardano.Serialization.Lib as Csl
import Data.Either (note)
import Data.Enum.Generic (genericFromEnum, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

data MIRPot = Reserves | Treasury

derive instance Eq MIRPot
derive instance Ord MIRPot
derive instance Generic MIRPot _

instance Show MIRPot where
  show = genericShow

instance EncodeAeson MIRPot where
  encodeAeson = toInt >>> encodeAeson

instance DecodeAeson MIRPot where
  decodeAeson = note (TypeMismatch "MIRPot") <<< fromInt <=< decodeAeson

toInt :: MIRPot -> Int
toInt = genericFromEnum <<< Csl.fromCslEnum <<< toCsl

fromInt :: Int -> Maybe MIRPot
fromInt = map (fromCsl <<< Csl.toCslEnum) <<< genericToEnum

toCsl :: MIRPot -> Csl.MIRPot
toCsl pot =
  Csl.toCslEnum $
    case pot of
      Reserves -> Csl.MIRPot_Reserves
      Treasury -> Csl.MIRPot_Treasury

fromCsl :: Csl.MIRPot -> MIRPot
fromCsl cslPot =
  case Csl.fromCslEnum cslPot of
    Csl.MIRPot_Reserves -> Reserves
    Csl.MIRPot_Treasury -> Treasury
