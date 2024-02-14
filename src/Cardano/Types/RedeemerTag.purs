module Cardano.Types.RedeemerTag
  ( RedeemerTag
      ( Spend
      , Mint
      , Cert
      , Reward
      )
  , fromCsl
  , toCsl
  , fromInt
  , toInt
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)

data RedeemerTag
  = Spend
  | Mint
  | Cert
  | Reward

derive instance Generic RedeemerTag _
derive instance Eq RedeemerTag
derive instance Ord RedeemerTag

instance Show RedeemerTag where
  show = genericShow

instance EncodeAeson RedeemerTag where
  encodeAeson = toCsl >>> encodeAeson

instance DecodeAeson RedeemerTag where
  decodeAeson = map fromCsl <<< decodeAeson

instance AsCbor RedeemerTag where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

toInt :: RedeemerTag -> Int
toInt = case _ of
  Spend -> 0
  Mint -> 1
  Cert -> 2
  Reward -> 3

fromInt :: Int -> Maybe RedeemerTag
fromInt = case _ of
  0 -> Just Spend
  1 -> Just Mint
  2 -> Just Cert
  3 -> Just Reward
  _ -> Nothing

fromCsl :: Csl.RedeemerTag -> RedeemerTag
fromCsl rt =
  unsafePartial $ fromJust $ fromInt $ fromJust $ Int.fromNumber $ Csl.redeemerTag_kind rt

toCsl :: RedeemerTag -> Csl.RedeemerTag
toCsl = case _ of
  Spend -> Csl.redeemerTag_newSpend
  Mint -> Csl.redeemerTag_newMint
  Cert -> Csl.redeemerTag_newCert
  Reward -> Csl.redeemerTag_newReward
