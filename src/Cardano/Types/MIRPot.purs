module Cardano.Types.MIRPot where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , encodeAeson
  )
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
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

toInt :: MIRPot -> Int -- encoding from CSL sources
toInt Reserves = 0
toInt Treasury = 1

fromInt :: Int -> Maybe MIRPot
fromInt 0 = Just Reserves
fromInt 1 = Just Treasury
fromInt _ = Nothing
